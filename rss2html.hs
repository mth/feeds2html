import Control.Exception as E
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import System.Environment
import System.Posix.Directory
import System.Posix.Files
import System.Posix.User
import qualified Network.HTTP as W
import Network.HTTP.Date
import Network.URI

tryIO :: IO a -> IO (Either E.IOException a)
tryIO = E.try

httpGet :: String -> [W.Header] -> IO (W.Response C.ByteString)
httpGet url headers = do
    uri <- maybe (fail $ "Bad URI: " ++ url) return (parseURI url)
    let request = W.Request uri W.GET headers C.empty
    W.simpleHTTP request >>= either (fail . show) return

ifModified time =
    [W.Header W.HdrIfModifiedSince
     (C.unpack $ formatHTTPDate $ epochTimeToHTTPDate time)]

fetchCachedImpl url filename = do
    stat <- tryIO (getFileStatus filename)
    let headers = either (const []) (ifModified . modificationTime) stat
    result <- httpGet url headers
    print (W.rspCode result)
    case W.rspCode result of
        (2, 0, 0) -> do
            C.writeFile filename (W.rspBody result)
            return (W.rspBody result)
        (3, 0, 4) -> C.readFile filename
        _ -> fail (W.rspReason result)

fetchCached url = do
    uid <- getRealUserID
    let tmpdir = "/tmp/.rss-cache-" ++ show uid
    tryIO $ createDirectory tmpdir 0o700
    let filename = tmpdir ++ '/' : (showDigest $ sha224 $ CL.pack url)
    fetchCachedImpl url filename

main = do
    args <- getArgs
    fetchCached (head args) >>= C.putStrLn
