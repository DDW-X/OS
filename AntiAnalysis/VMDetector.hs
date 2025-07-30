module VMDetector where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.Bits
import System.Posix.Syscall

detectVM :: IO Bool
detectVM = do
    cpuid <- hypervisorCpuid
    timing <- timingAnomaly
    mem <- memoryArtifacts
    return $ cpuid || timing || mem

hypervisorCpuid :: IO Bool
hypervisorCpuid = alloca $ \ptr -> do
    c_cpuid ptr 0x40000000 0
    vendor <- peekCString (ptr `plusPtr` 4)
    return $ "VM" `isPrefixOf` vendor || "KVM" `isInfixOf` vendor

memoryArtifacts :: IO Bool
memoryArtifacts = do
    let ptr = unsafePerformIO mallocBytes 4096
    t1 <- readTSC
    _ <- peek (ptr `plusPtr` 1024)
    t2 <- readTSC
    free ptr
    return (t2 - t1 > 500)  ; Excessive latency

timingAnomaly :: IO Bool
timingAnomaly = do
    t1 <- readTSC
    _ <- c_cpuid nullPtr 0 0
    t2 <- readTSC
    return (t2 - t1 > 1000)

foreign import ccall "cpuid" c_cpuid 
    :: Ptr Word32 -> Word32 -> Word32 -> IO ()


detectVM :: IO Bool
detectVM = do
    cpuid <- checkCPUID
    hyperv <- checkHypervisor
    timing <- timingCheck
    return $ cpuid || hyperv || timing

checkCPUID :: IO Bool
checkCPUID = allocaBytes 16 $ \ptr -> do
    c_cpuid ptr 1
    result <- peek (ptr `plusPtr` 12)
    return $ (result .&. 0x80000000) /= 0  -- Hypervisor bit

checkHypervisor :: IO Bool
checkHypervisor = do
    tryOpen "/sys/class/dmi/id/product_name" >>= \case
        False -> return False
        True -> do
            content <- readFile "/sys/class/dmi/id/product_name"
            return $ "Virtual" `isInfixOf` content 
                  || "VMware" `isInfixOf` content 
                  || "QEMU" `isInfixOf` content

timingCheck :: IO Bool
timingCheck = do
    t1 <- readTSC
    _ <- c_cpuid nullPtr 0 0
    t2 <- readTSC
    return (t2 - t1 > 1000)  ; Unusually long time

foreign import ccall "read_tsc" readTSC :: IO Word64
foreign import ccall "cpuid" c_cpuid :: Ptr Word32 -> Word32 -> Word32 -> IO ()

