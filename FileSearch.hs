

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension )
import System.Environment (getArgs)

import Debug.Trace

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

simpleFind p path = do
  names <- getRecursiveContents path
  let z = filter p names
  return (map (drop (1+length path)) z)

extension c p = takeExtension p == '.':c 

main = do
  (a:b:_) <- getArgs
-- GOOD ONE!
  mapM_ putStrLn =<< (simpleFind (extension a) b)

