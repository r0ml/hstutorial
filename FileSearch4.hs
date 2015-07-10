{-


import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension )
import System.Environment (getArgs)
import Debug.Trace
-}

import Preface.R0ml
import Control.Concurrent.Async


getRecursiveContents :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getRecursiveContents f topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
--  let throttled name = bracket_ (waitQSem qs) (signalQSem qs) $ do
  let unthrottled name = do
         let path = topdir </> name
         -- putStrLn path
         isDirectory <- doesDirectoryExist path
         if isDirectory
            then getRecursiveContents f path
            else return [path | f path]
  as <- mapM (async . unthrottled) properNames
  rs <- mapM wait as
  return (concat rs)


simpleFind p path = do
  names <- getRecursiveContents p path
  -- let z = filter p names
  return (map (drop (1+length path)) names)

extension c p = takeExtension p == '.':c 

main = do
  (a:b:_) <- getArgs
-- GOOD ONE!
  mapM_ putStrLn =<< (simpleFind (extension a) b)

