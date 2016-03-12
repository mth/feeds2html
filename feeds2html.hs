{-
 -  HTTPS proxy.
 -
 -  This program is free software: you can redistribute it and/or modify
 -  it under the terms of the GNU General Public License as published by
 -  the Free Software Foundation, either version 3 of the License, or
 -  (at your option) any later version.
 -
 -  This program is distributed in the hope that it will be useful,
 -  but WITHOUT ANY WARRANTY; without even the implied warranty of
 -  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -  GNU General Public License for more details.
 -
 -  You should have received a copy of the GNU General Public License
 -  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}
import Control.Concurrent
import Control.Exception as E
import Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Char
import Data.Digest.Pure.SHA
import Data.Function
import Data.List (sortBy)
import Data.Maybe
import Data.Time (UTCTime, utctDay, utctDayTime, addUTCTime,
                  formatTime, parseTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Date
import Network.URI
import System.Environment
import System.Exit
import System.IO
import System.Locale (defaultTimeLocale, iso8601DateFormat, rfc822DateFormat)
import System.Posix (createDirectory, getRealUserID)
import System.Posix.Files
import System.Process
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import
import Text.Feed.Query
import qualified Text.Feed.Types as Feed
import Text.HTML.SanitizeXSS
import Text.HTML.TagSoup
import Text.XML.Light

data FeedOption = PreserveOrder | Adjust Double | MaxItems Int | Skip Int
    deriving (Read, Eq)

data HtmlDef = H C.ByteString | Items | Errors C.ByteString C.ByteString |
               Title | Link | Host | Time String | Summary | Nth
    deriving Read

data ConfigItem =
    Feed C.ByteString [FeedOption] |
    Item [HtmlDef] |
    Page [HtmlDef] |
    MaxAge Int |
    Limit Int
    deriving Read

data Config = Config { feeds  :: [(String, [FeedOption])],
                       item   :: [HtmlDef],
                       page   :: [HtmlDef],
                       maxAge :: Int,
                       limit  :: Maybe Int }

-- all text fields are UTF-8 encoded
data Entry = Entry { title    :: !C.ByteString,
                     link     :: !C.ByteString,
                     time     :: Maybe UTCTime,
                     summary  :: !C.ByteString,
                     score    :: !Double,
                     nth      :: !Int } deriving Show

tryIO :: IO a -> IO (Either E.IOException a)
tryIO = E.try

parseConfigItems str = skip parse 1 str
  where skip _ line ('\n':s) = skip parse (line + 1) s
        skip tr line (c:s) | isSpace c = skip tr line s
        skip _ _ "" = Config { feeds = [], item = [], page = [],
                               maxAge = 300, limit = Nothing }
        skip tr line str = tr line str
        parse line str = case reads str of
            ((result, tail):_) ->
                let len = length str - length tail in
                let !nl = line + length (filter (== '\n') (take len str)) in
                compose result (skip err nl tail)
            _ -> error (show line ++ ": syntax error")
        err line _ = error (show line ++ ": expected newline after definition")
        compose cfgitem cfg = case cfgitem of
            Feed url opt -> cfg { feeds  = (C.unpack url, opt) : feeds cfg }
            Page html    -> cfg { page   = html ++ page cfg }
            Item html    -> cfg { item   = html ++ item cfg }
            MaxAge age   -> cfg { maxAge = age }
            Limit limit  -> cfg { limit  = Just limit }

readConfig = fmap parse . C.readFile
  where parse = parseConfigItems . C.unpack . C.unlines .
                filter notComment . C.lines
        notComment s = let s' = C.dropWhile isSpace s in
                       C.null s' || C.head s' /= '#'

escapeXml str = showCData (CData CDataText str Nothing)
maybeStr = maybe C.empty (fromString . escapeXml)

dateFormats = map (maybe Nothing . parseTime defaultTimeLocale)
    [ rfc822DateFormat, iso8601DateFormat (Just "%H:%M:%S%Z"),
      iso8601DateFormat (Just "%H:%M:%S%Q%Z") ]

timeToScore t =
    fromIntegral (fromEnum (utctDay t) - 50000) * 1440 +
    realToFrac (utctDayTime t) / 60

tryLink item =
    case getItemLink item of
        Just s | s /= "" -> Just s
        _ -> extract item
  where extract (Feed.AtomItem atom) =
            let links = Atom.entryLinks atom
                strs = map atomLink links ++ map Atom.linkHref links in
            listToMaybe $  filter (/= "") strs
        extract _ = Nothing
        atomLink link | Atom.linkType link == Nothing = Atom.linkHref link
        atomLink _ = ""

trySummary item =
    case getItemDescription item of
        Just s | s /= "" -> html s
        _ -> extract item
  where extract (Feed.AtomItem atom) =
            case Atom.entryContent atom of
                Just (Atom.TextContent c) -> maybeStr (Just c)
                Just (Atom.HTMLContent c) -> html c
                Just (Atom.XHTMLContent c) -> html $ showElement c
                Just (Atom.MixedContent str c) ->
                    C.append (maybeStr str) (html $ concatMap showContent c)
                _ -> C.empty
        extract _ = C.empty
        html = encodeUtf8 . sanitize . T.pack
        sanitize = filterTags (filter allowedTag) . sanitizeBalance
        allowedTag (TagOpen name _) = not (elem name disallow)
        allowedTag (TagClose name)  = not (elem name disallow)
        allowedTag (TagComment _)   = False
        allowedTag _ = True
        disallow = map T.pack ["br", "p", "div", "img", "h1", "h2", "h3", "h4"]

getEntry item = Entry {
    title     = fst $ C.spanEnd isSpace $ C.dropWhile isSpace
                    $ maybeStr (getItemTitle item),
    link      = maybeStr $ tryLink item,
    time      = time,
    summary   = trySummary item,
    score     = maybe 0.0 timeToScore time,
    nth       = 0
} where time = listToMaybe (mapMaybe ($ getItemDate item) dateFormats)

runFork action = do
    result <- newEmptyMVar
    forkIO (action >>= putMVar result)
    return result

wget :: String -> [String] -> IO (Either C.ByteString C.ByteString)
wget url headers = do
    let param = "-nv" : "-O" : "-" : "--no-check-certificate" :
            "--timeout=10" : (map ("--header=" ++) headers ++ [url])
    (inp, out, err, pid) <- runInteractiveProcess "wget" param Nothing Nothing
    hClose inp
    error  <- runFork (C.hGetContents err)
    output <- C.hGetContents out
    exitCode <- waitForProcess pid
    case exitCode of
         ExitSuccess -> return (Right output)
         _ -> takeMVar error >>= return . Left

ifModified time =
    ["If-Modified-Since: " ++
     (C.unpack $ formatHTTPDate $ epochTimeToHTTPDate time)]

fetchCached url filename = do
    stat <- tryIO (getFileStatus filename)
    let headers = either (const []) (ifModified . modificationTime) stat
    result <- wget url headers
    case result of
        Left error ->
            if C.pack " 304:" `C.isInfixOf` error
                then C.readFile filename
                else fail (C.unpack error)
        Right content -> do
            C.writeFile filename content
            return content

getCacheFile key = do
    uid <- getRealUserID
    let tmpdir = "/tmp/.rss-cache-" ++ show uid
    tryIO $ createDirectory tmpdir 0o700
    return $ tmpdir ++ '/' : (showDigest $ sha224 $ CL.pack key)

adjustScores maxScore = opt
  where order _ [] = []
        order best (item : newer) =
            let newer' = order (max best (score item)) newer in
            if score item > best
               then item : newer'
               else let next_score = case newer' of
                                        h : _ -> score h
                                        [] -> max best maxScore in
                    item { score = (best + next_score) / 2 } : newer'
        opt (PreserveOrder : r) l = opt r (reverse (order (-1e9) (reverse l)))
        opt (MaxItems n : r) l = opt r (take n l)
        opt (Skip n : r) l = opt r (drop n l)
        opt (Adjust by : r) l =
            map (\e -> e { score = score e + by * 60 }) (opt r l)
        opt [] l = l

toEntries curtime options =
    adjustScores (timeToScore curtime) options . map getEntry . feedItems

parseFeed =
    maybe (Left "feed parse error") Right . parseFeedString . toString

fetchFeed (url, options) = do
    xml <- tryIO (getCacheFile url >>= fetchCached url)
    case either (Left . show) parseFeed xml of
        Right feed -> do t <- getCurrentTime
                         return ([], toEntries t options feed)
        Left error -> return ([error], [])

fetchFeeds feeds = do
    results <- mapM (runFork . fetchFeed) feeds >>= mapM readMVar
    let entries = sortBy (on (flip compare) score)
                         (concatMap snd results)
        setNth entry nth = entry { nth = nth }
    return (concatMap fst results, zipWith setNth entries [1..])

toHtml cfg (errors, allItems) = C.concat $ htmlOf items (page cfg)
  where htmlOf = concatMap . flip process
        process template = case template of
            H html -> const [html]
            Items -> concatMap ((`htmlOf` item cfg) . (:[]))
            Title -> map title
            Link -> map link
            Host -> map (C.pack . host . C.unpack . link)
            Time format -> mapMaybe (fmap (timeStr format) . time)
            Summary -> map summary
            Nth -> map (C.pack . show . nth)
            Errors before after ->
                let error e = [before, C.pack (escapeXml e), after] in
                const (concatMap error errors)
        timeStr f = C.pack . formatTime defaultTimeLocale f
        host uri = maybe "" uriRegName (parseURI uri >>= uriAuthority)
        items = maybe allItems (`take` allItems) (limit cfg)

-- basically same UTCTime returning variant as in newer System.Directory
getModificationTime file = do
    stat <- getFileStatus file
    return $ posixSecondsToUTCTime $ realToFrac $ modificationTime stat

getCachedHtml cfgFile = do
    cfg <- readConfig cfgFile
    cache <- getCacheFile cfgFile
    cacheStamp <- tryIO $ getModificationTime cache
    useCached <- case cacheStamp of
        Right cached -> do
            cfgStamp <- getModificationTime cfgFile
            currentTime <- getCurrentTime
            return $ cfgStamp < cached &&
                     currentTime < addUTCTime (fromIntegral $ maxAge cfg) cached
        Left _ -> return False
    if useCached
        then C.readFile cache
        else do html <- toHtml cfg `fmap` fetchFeeds (feeds cfg)
                C.writeFile cache html
                return html
main = do
    args <- getArgs
    getCachedHtml (head args) >>= C.putStrLn
