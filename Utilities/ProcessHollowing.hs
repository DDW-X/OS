-- ProcessHollowing.hs
-- Auto-generated scaffold.

module Utilities.ProcessHollowing where

import System.Process
import System.Posix.Process
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.ByteString as BS

hollowProcess :: FilePath -> IO ()
hollowProcess target = do
    pid <- forkProcess $ do
        executeTarget target
        exitImmediately ExitSuccess
    return ()

executeTarget :: FilePath -> IO ()
executeTarget path = do
    targetContent <- BS.readFile path
    ptr <- allocateExecutable (BS.length targetContent)
    BS.useAsCStringLen targetContent $ \(cstr, len) ->
        copyBytes ptr cstr len
    let func = castPtrToFunPtr ptr :: FunPtr (IO ())
    callFunPtr func
    