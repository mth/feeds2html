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
import Text.Feed.Import
import Text.Feed.Query
import Text.HTML.SanitizeXSS
import Text.HTML.TagSoup
import Text.XML.Light

data FeedOption = PreserveOrder | Adjust Double deriving (Read, Eq)
data HtmlDef = H C.ByteString | Items | Errors C.ByteString C.ByteString |
               Title | Link | Host | Time String | Summary
    deriving Read

data ConfigItem =
    Feed C.ByteString [FeedOption] |
    Item [HtmlDef] |
    Page [HtmlDef] |
    MaxAge Int
    deriving Read

data Config = Config { feeds  :: [(String, [FeedOption])],
                       item   :: [HtmlDef],
                       page   :: [HtmlDef],
                       maxAge :: Int }

-- all text fields are UTF-8 encoded
data Entry = Entry { title    :: !C.ByteString,
                     link     :: !C.ByteString,
                     time     :: Maybe UTCTime,
                     summary  :: !C.ByteString,
                     score    :: !Double } deriving Show

tryIO :: IO a -> IO (Either E.IOException a)
tryIO = E.try

parseConfigItems str = skip parse 1 str
  where skip _ line ('\n':s) = skip parse (line + 1) s
        skip tr line (c:s) | isSpace c = skip tr line s
        skip _ _ "" = Config { feeds = [], item = [], page = [], maxAge = 300 }
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

getEntry item = Entry {
    title     = fst $ C.spanEnd isSpace $ C.dropWhile isSpace
                    $ maybeStr (getItemTitle item),
    link      = maybeStr (getItemLink item),
    time      = time,
    summary   = maybe C.empty (encodeUtf8 . sanitize . T.pack)
                      (getItemDescription item),
    score     = maybe 0.0 timeToScore time
} where time = listToMaybe (mapMaybe ($ getItemDate item) dateFormats)
        sanitize = filterTags (filter allowedTag) . sanitizeBalance
        allowedTag (TagOpen name _) = not (elem name disallow)
        allowedTag (TagClose name)  = not (elem name disallow)
        allowedTag (TagComment _)   = False
        allowedTag _ = True
        disallow = [T.pack "br", T.pack "p", T.pack "div"]

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

adjustScores maxScore options = foldr (.) orderf (map optf options)
  where order _ [] = []
        order best (item : newer) =
            let newer' = order (max best (score item)) newer in
            if score item > best
               then item : newer'
               else let next_score = case newer' of
                                        h : _ -> score h
                                        [] -> max best maxScore in
                    item { score = (best + next_score) / 2 } : newer'
        orderf = if elem PreserveOrder options
                     then reverse . order (-1e9) . reverse else id
        optf (Adjust by) = map (\e -> e { score = score e + by * 60 })
        optf PreserveOrder = id

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
    return (concatMap fst results, entries)

toHtml cfg (errors, items) = C.concat $ htmlOf items (page cfg)
  where htmlOf = concatMap . flip process
        process template = case template of
            H html -> const [html]
            Items -> concatMap ((`htmlOf` item cfg) . (:[]))
            Title -> map title
            Link -> map link
            Host -> map (C.pack . host . C.unpack . link)
            Time format -> mapMaybe (fmap (timeStr format) . time)
            Summary -> map summary
            Errors before after ->
                let error e = [before, C.pack (escapeXml e), after] in
                const (concatMap error errors)
        timeStr f = C.pack . formatTime defaultTimeLocale f
        host uri = maybe "" uriRegName (parseURI uri >>= uriAuthority)

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
