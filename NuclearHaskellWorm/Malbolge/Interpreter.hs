-- Interpreter.hs
-- Auto-generated scaffold.

module Malbolge.Interpreter where

import Data.Bits
import Data.Word
import Data.Array.IO
import Control.Monad
import Foreign.Ptr
import System.IO.Unsafe

type MalbolgeState = (IOArray Int Word32, Word32, Word32, Word32)

crazy :: Word32 -> Word32 -> Word32
crazy x y = foldl' step 0 [0..15]
  where
    step acc i = 
        let shift = 2*i
            xBits = (x `shiftR` shift) .&. 3
            yBits = (y `shiftR` shift) .&. 1
            bit = case xBits of
                0 -> yBits
                1 -> 1 - yBits
                2 -> yBits `xor` 1
                3 -> (x `shiftR` shift .&. 1) `xor` yBits
        in acc .|. (bit `shiftL` shift)

executeMalbolge :: [Word32] -> IO ()
executeMalbolge program = do
    mem <- newArray (0, 59048) 0 :: IO (IOArray Int Word32)
    forM_ (zip [0..] program) $ \(i, val) ->
        writeArray mem i val
        
    let run (mem, a, c, d) = do
            instr <- readArray mem (fromIntegral c)
            let op = crazy instr a `mod` 94
            case op of
                4 -> do  -- Rotate
                    val <- readArray mem (fromIntegral (c + d) `mod` 59049)
                    let newVal = crazy val a
                    writeArray mem (fromIntegral (c + d) `mod` 59049) newVal
                    next (c+1, d+1)
                23 -> do  -- Jump
                    next (a `mod` 59049, a)
                39 -> when (a /= 0) $ next (d, d)
                _ -> next (c+1, d)
        next (c', d') = run (mem, a, c', d')
    
    run (mem, 0, 0, 0)
    