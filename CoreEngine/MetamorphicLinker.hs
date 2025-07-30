module MetamorphicLinker where

import Foreign.Ptr
import Foreign.Marshal.Array
import Data.ByteString as BS
import Data.Bits
import System.Random
import Control.Monad
import EntropyHarvester

-- Randomly reassemble code blocks
reassemble :: BS.ByteString -> IO BS.ByteString
reassemble code = do
    blocks <- splitBlocks code
    shuffled <- shuffleBlocks blocks
    return $ BS.concat shuffled

splitBlocks :: BS.ByteString -> IO [BS.ByteString]
splitBlocks bs = do
    blockSize <- randomRIO (16, 256)
    return $ unfoldr split blockSize bs
  where
    split size bytes
        | BS.null bytes = Nothing
        | otherwise = Just (BS.splitAt size bytes)

shuffleBlocks :: [BS.ByteString] -> IO [BS.ByteString]
shuffleBlocks blocks = do
    entropy <- harvestEntropy
    let gen = mkStdGen (fromIntegral entropy)
    return $ shuffle' blocks (length blocks) gen

-- Insert branch misalignment
insertJumps :: BS.ByteString -> IO BS.ByteString
insertJumps code = do
    let len = BS.length code
    positions <- replicateM 10 (randomRIO (0, len-5))
    foldM insertJump code positions
  where
    insertJump bs pos = do
        jumpType <- randomRIO (0, 2)
        let jump = case jumpType of
              0 -> BS.pack [0xEB, 0x00]  -- JMP +2
              1 -> BS.pack [0x74, 0x00]  -- JE +2
              _ -> BS.pack [0x75, 0x00]  -- JNE +2
        return $ BS.take pos bs <> jump <> BS.drop (pos+2) bs
        