{-# LANGUAGE ForeignFunctionInterface #-}
module EntropyHarvester where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Data.Word
import System.Clock
import GHC.Stats

foreign import ccall "get_rdrand" c_getRdrand :: IO Word64
foreign import ccall "get_rdseed" c_getRdseed :: IO Word64

harvestEntropy :: IO Word128
harvestEntropy = do
    tsc <- getTime
    mem <- getMemoryStats
    pid <- getProcessID
    rdrand <- c_getRdrand
    rdseed <- c_getRdseed
    let !entropy = combineEntropy tsc mem pid rdrand rdseed
    return entropy

combineEntropy :: Word64 -> GCStats -> Word32 -> Word64 -> Word64 -> Word128
combineEntropy tsc mem pid rdrand rdseed = 
    fromIntegral tsc * 0x123456789ABCDEF0 
    ^ fromIntegral (max_live_bytes mem) 
    ^ fromIntegral pid 
    ^ fromIntegral rdrand 
    ^ fromIntegral rdseed

getMemoryStats :: IO GCStats
getMemoryStats = do
    stats <- getGCStats
    return stats

getTime :: IO Word64
getTime = do
    tsc <- readTime
    return tsc

foreign import ccall "
static unsigned long long readTime() {
    unsigned int lo, hi;
    asm volatile (\"rdtsc\" : \"=a\"(lo), \"=d\"(hi));
    return ((unsigned long long)hi << 32) | lo;
}" readTime :: IO Word64
