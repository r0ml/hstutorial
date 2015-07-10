{-
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension )
import System.Environment (getArgs)
import System.Posix (getFileStatus, getSymbolicLinkStatus, isSymbolicLink)

import Control.Exception (bracket_)
import Control.Concurrent.QSem
import Data.Either

import Debug.Trace
-}

import Preface.R0ml
import Control.Concurrent.Async

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
  -- return (map (drop (1+length path)) z)
  return z

extension c p = takeExtension p == '.':c 

isLink p = isSymbolicLink <$> getSymbolicLinkStatus p


main = do
  (a:b:_) <- getArgs
-- GOOD ONE!
  qs <- newQSem (read a :: Int) 
  let throttled n = bracket_ (waitQSem qs) (signalQSem qs) (isLink n)
  nms <- getRecursiveContents b
  as <- mapM (async . throttled) nms
  rs <- mapM wait as
  mapM_ putStrLn ( map snd ( filter fst (zip rs nms)))

  
--   mapM_ putStrLn =<< (simpleFind (extension a) b)

