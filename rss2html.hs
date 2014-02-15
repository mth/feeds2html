import Control.Concurrent
import Control.Exception as E
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Digest.Pure.SHA
import Network.HTTP.Date
import System.Environment
import System.Exit
import System.IO
import System.Posix.Directory
import System.Posix.Files
import System.Posix.User
import System.Process
import Text.Feed.Import
import Text.Feed.Query

tryIO :: IO a -> IO (Either E.IOException a)
tryIO = E.try

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
    maybe (Left "feed parse error") Right . parseFeedString . toString

fetchFeed url =
    either (Left . show) parseFeed `fmap` tryIO (fetchCached url)

fetchFeeds urls =
    mapM (runFork . fetchFeed) urls >>= mapM readMVar

main = do
    args <- getArgs
    fetchFeeds args >>= mapM print
