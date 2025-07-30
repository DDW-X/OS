-- Encryption.hs
-- Auto-generated scaffold.

module Utilities.Encryption where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Data.ByteArray (convert)
import System.Entropy

generateAESKey :: IO BS.ByteString
generateAESKey = getEntropy 32  -- AES-256

generateIV :: IO BS.ByteString
generateIV = getEntropy 16

encryptWorm :: BS.ByteString -> IO BS.ByteString
encryptWorm worm = do
    key <- generateAESKey
    iv <- generateIV
    let cipher = throwCryptoError (cipherInit key :: CryptoError AES)
    return $ ctrCombine cipher iv worm

decryptWorm :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
decryptWorm key iv encrypted = 
    let cipher = throwCryptoError (cipherInit key :: CryptoError AES)
    in ctrCombine cipher iv encrypted
    