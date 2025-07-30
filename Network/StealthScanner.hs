-- StealthScanner.hs
-- Auto-generated scaffold.

module Network.StealthScanner where

import Network.Socket
import Control.Concurrent
import Data.List
import System.Random

scanPorts :: String -> [PortNumber] -> IO [PortNumber]
scanPorts host ports = do
    delays <- replicateM (length ports) (randomRIO (100000, 500000))  -- تاخیر تصادفی
    results <- mapM (\(port, delay) -> do
        threadDelay delay
        checkPort host port) (zip ports delays)
    return [port | (port, True) <- zip ports results]

checkPort :: String -> PortNumber -> IO Bool
checkPort host port = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    addr <- inet_addr host
    let sockAddr = SockAddrInet port addr
    result <- tryConnect sock sockAddr
    close sock
    return result

tryConnect :: Socket -> SockAddr -> IO Bool
tryConnect sock addr = do
    result <- try $ connect sock addr
    case result of
        Right () -> return True
        Left _   -> return False
        