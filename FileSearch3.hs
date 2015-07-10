

{-

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension )
import System.Environment (getArgs, getEnv)
import System.Posix (getFileStatus, getSymbolicLinkStatus, isSymbolicLink)

import Control.Exception (bracket_)
import Control.Concurrent.QSem
import Data.Either

import Debug.Trace
-}

import Preface.R0ml
import Control.Concurrent.Async

getRecursiveContents :: QSem -> FilePath -> IO [FilePath]
getRecursiveContents qs topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
--  let throttled name = bracket_ (waitQSem qs) (signalQSem qs) $ do
  let unthrottled name = do
         let path = topdir </> name
         -- putStrLn path
         isDirectory <- doesDirectoryExist path
         if isDirectory
            then getRecursiveContents qs path
            else return [path]
  as <- mapM (async . unthrottled) properNames
  rs <- mapM wait as
  return (concat rs)

simpleFind p path = do
  qs <- newQSem 100
  names <- getRecursiveContents qs path
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
  nms <- getRecursiveContents qs b
  as <- mapM (async . throttled) nms
  rs <- mapM wait as
  mapM_ putStrLn ( map snd ( filter fst (zip rs nms)))

  
--   mapM_ putStrLn =<< (simpleFind (extension a) b)

