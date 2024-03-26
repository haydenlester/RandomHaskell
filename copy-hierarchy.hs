import System.Directory
import System.Environment
import System.FilePath
import Control.Monad

logged :: FilePath -> IO ()
logged fp = do
  exists <- doesDirectoryExist fp
  unless exists $ do
    createDirectory fp
    putStrLn $ "Created: " ++ fp

refresh :: FilePath -> FilePath -> IO ()
refresh src dest = do
  putStrLn $ "Syncing hierarchy: " ++ src ++ " to " ++ dest
  logged dest
  contents <- listDirectory src
  mapM_ (go dest src) contents

go :: FilePath -> FilePath -> FilePath -> IO ()
go dest src name = do
  let destPath = dest </> name
      srcPath  = src  </> name
  logged destPath
  contents <- listDirectory srcPath
  mapM_ (go destPath srcPath) contents
