
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
    xs <- catch gacc ((\e -> return [] )::SomeException -> IO [a])
    return (x : xs)

getRecursiveContents :: Int -> FilePath -> TChan (String, Integer) -> IO ()
getRecursiveContents n topdir ochan = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
--  let throttled name = bracket_ (waitQSem qs) (signalQSem qs) $ do
      unthrottled name = do
         let path = topdir </> name
         -- putStrLn path
         isDirectory <- doesDirectoryExist path
         if isDirectory
            then do 
                -- atomically $ writeTChan ochan (Left True) 
                ti <- forkIO $ getRecursiveContents n path ochan
                -- traceShow ti (return ())
                return ()
            else getFileSize path >>= \x -> atomically (writeTChan ochan ((drop n path), x))
   in mapM_ unthrottled properNames -- >> atomically (writeTChan ochan (Left False))
      
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

-- GOOD ONE!
  mapM_ print =<< simpleFind a

