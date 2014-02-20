import Control.Concurrent
import Control.Exception as E
import Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Char
import Data.Digest.Pure.SHA
import Data.Maybe
import Data.Time (UTCTime, utctDay, utctDayTime, parseTime)
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

data FeedOption = PreserveOrder deriving Read
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

getEntry item = Entry {
    title     = maybeStr (getItemTitle item),
    link      = maybeStr (getItemLink item),
    time      = time,
    descr     = maybeStr (getItemDescription item),
    score     = maybe 0.0 score time
} where time = listToMaybe (mapMaybe ($ getItemDate item) dateFormats)
        score t = fromIntegral (fromEnum (utctDay t)) * 1440 +
                  fromRational (toRational (utctDayTime t) / 60)

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

parseFeed =
    maybe (Left "feed parse error")
          (Right . map getEntry . feedItems) . parseFeedString . toString

fetchFeed url =
    either (Left . show) parseFeed `fmap` tryIO (fetchCached url)

fetchFeeds urls =
    mapM (runFork . fetchFeed) urls >>= mapM readMVar

main = do
    args <- getArgs
    cfg <- readConfig (head args)
    fetchFeeds (map fst (feeds cfg)) >>= mapM print
