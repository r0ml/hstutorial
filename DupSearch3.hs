{-# OPTIONS_GHC -threaded #-}

{-
-- import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension )
import System.Posix (getFileStatus, fileSize)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Debug.Trace
import Control.Exception (SomeException, try, catch)
-}

import Preface.R0ml

getFileSize :: FilePath -> IO Integer
getFileSize path = (fromIntegral . fileSize) <$> getFileStatus path

getAllChanContents :: TChan a -> IO [a]
getAllChanContents ch = gacc where
  gacc = do
    x <- atomically $ readTChan ch
    xs <- catch gacc ((\e -> traceShow e $ return [] )::SomeException -> IO [a])
    return (x : xs)

getRecursiveContents :: Int -> FilePath -> TChan (String, Integer) -> IO ()
getRecursiveContents n topdir ochan = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
      unthrottled name = do
         let path = topdir </> name
         isDirectory <- doesDirectoryExist path
         isFile <- doesFileExist path
         if isDirectory
            then do 
                ti <- forkIO $ getRecursiveContents n path ochan
                return ()
            else if isFile then getFileSize path >>= \x -> atomically (writeTChan ochan ((drop n path), x)) else return ()
   in mapM_ unthrottled properNames 
      
simpleFind path = do
  j <- newTChanIO 
  getRecursiveContents (1+length path) path j
  -- let z = filter p names
  getAllChanContents j

extension c p = takeExtension p == '.':c 

main = do
  aa <- getArgs
  a <- case aa of 
    (aa:_) -> return aa
    _ -> putStrLn "missing argument: specify directory" >> exitFailure >> return "failed"
  mapM_ print =<< simpleFind a

