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
import Data.Time (UTCTime, utctDay, utctDayTime, parseTime, getCurrentTime)
import Network.HTTP.Date
import System.Environment
import System.Exit
import System.IO
import System.Locale (defaultTimeLocale, iso8601DateFormat, rfc822DateFormat)
import System.Posix (createDirectory, getRealUserID)
import System.Posix.Files
import System.Process
import Text.Feed.Import
import Text.Feed.Query

data FeedOption = PreserveOrder | Adjust Double deriving (Read, Eq)
data HtmlDef = H C.ByteString | Title deriving Read

data ConfigItem =
    Feed C.ByteString [FeedOption] |
    Item [HtmlDef] |
    Page [HtmlDef]
    deriving Read

data Config = Config { feeds :: [(String, [FeedOption])],
                       items :: [[HtmlDef]],
                       page  :: [HtmlDef] }

data Entry = Entry { title    :: !C.ByteString,
                     link     :: !C.ByteString,
                     time     :: Maybe UTCTime,
                     descr    :: !C.ByteString,
                     score    :: !Double } deriving Show

tryIO :: IO a -> IO (Either E.IOException a)
tryIO = E.try

parseConfigItems str = skip parse 1 str
  where skip _ line ('\n':s) = skip parse (line + 1) s
        skip tr line (c:s) | isSpace c = skip tr line s
        skip _ _ "" = Config { feeds = [], items = [], page = [] }
        skip tr line str = tr line str
        parse line str = case reads str of
            ((result, tail):_) ->
                let len = length str - length tail in
                let !nl = line + length (filter (== '\n') (take len str)) in
                compose result (skip err nl tail)
            _ -> error (show line ++ ": syntax error")
        err line _ = error (show line ++ ": expected newline after definition")
        compose item cfg = case item of
            Feed url opt -> cfg { feeds = (C.unpack url, opt) : feeds cfg }
            Item html    -> cfg { items = html : items cfg }
            Page html    -> cfg { page  = html ++ page cfg }

readConfig = fmap parse . C.readFile
  where parse = parseConfigItems . C.unpack . C.unlines .
                filter notComment . C.lines
        notComment s = let s' = C.dropWhile isSpace s in
                       C.null s' || C.head s' /= '#'

maybeStr = maybe C.empty fromString

dateFormats = map (maybe Nothing . parseTime defaultTimeLocale)
    [ rfc822DateFormat, iso8601DateFormat (Just "%H:%M:%S%Z"),
      iso8601DateFormat (Just "%H:%M:%S%Q%Z") ]

timeToScore t =
    fromIntegral (fromEnum (utctDay t) - 50000) * 1440 +
    realToFrac (utctDayTime t) / 60

getEntry item = Entry {
    title     = maybeStr (getItemTitle item),
    link      = maybeStr (getItemLink item),
    time      = time,
    descr     = maybeStr (getItemDescription item),
    score     = maybe 0.0 timeToScore time
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
    output <- runFork (C.hGetContents out)
    error  <- runFork (C.hGetContents err)
    exitCode <- waitForProcess pid
    case exitCode of
         ExitSuccess -> takeMVar output >>= return . Right
         _ -> takeMVar error >>= return . Left

ifModified time =
    ["If-Modified-Since: " ++
     (C.unpack $ formatHTTPDate $ epochTimeToHTTPDate time)]

fetchCachedImpl url filename = do
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

fetchCached url = do
    uid <- getRealUserID
    let tmpdir = "/tmp/.rss-cache-" ++ show uid
    tryIO $ createDirectory tmpdir 0o700
    let filename = tmpdir ++ '/' : (showDigest $ sha224 $ CL.pack url)
    fetchCachedImpl url filename

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
    xml <- tryIO (fetchCached url)
    case either (Left . show) parseFeed xml of
        Right feed -> do t <- getCurrentTime
                         return ([], toEntries t options feed)
        Left error -> return ([error], [])

fetchFeeds feeds = do
    results <- mapM (runFork . fetchFeed) feeds >>= mapM readMVar
    let entries = sortBy (on (flip compare) score)
                         (concatMap snd results)
    return (concatMap fst results, entries)

main = do
    args <- getArgs
    cfg <- readConfig (head args)
    (errors, entries) <- fetchFeeds (feeds cfg)
    mapM print entries
    mapM putStrLn errors
