-- MalbolgeIntegration.hs
-- Auto-generated scaffold.

module Core.MalbolgeIntegration where

import Malbolge.Interpreter
import Malbolge.Payloads.embedded
import System.Info

executePlatformSpecificMalbolge :: IO ()
executePlatformSpecificMalbolge = do
    putStrLn "🔥 Activating Malbolge payload..."
    payload <- getPlatformPayload
    executeMalbolge payload

getPlatformPayload :: IO [Word32]
getPlatformPayload = do
    case os of
        "linux" -> return $ unpackBytes linuxX86Payload
        "mingw32" -> return $ unpackBytes winX64Payload
        _ -> return []

unpackBytes :: BS.ByteString -> [Word32]
unpackBytes bs = 
    let words32 = BS.unpack bs
    in map fromIntegral words32
    