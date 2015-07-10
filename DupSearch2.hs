{-
import Control.Monad (forM, when)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension )
import System.Posix (getFileStatus, fileSize)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import System.IO.Unsafe (unsafeInterleaveIO)
import Debug.Trace
import Control.Exception (catch, SomeException)
-}

import Preface.R0ml
import Control.Concurrent.Async

getFileSize :: FilePath -> IO Integer
-- getFileSize path = getFileStatus path >>= return . fromIntegral . fileSize
getFileSize path = (fromIntegral . fileSize) <$> getFileStatus path

getAllChanContents :: Chan (Either Bool a) -> IO [a]
getAllChanContents ch = gacc 0 where
  gacc n = do
    x <- readChan ch
    case x of 
      Left False -> {- traceShow ("false",n) $ -} if n == 1 then return [] else gacc (n-1)
      Left True -> {- traceShow ("true",n) $ -} gacc (n+1)
      Right a -> do
       xs <- gacc n
       return (a : xs)

getRecursiveContents :: Chan (Either Bool (String, Integer)) -> FilePath -> IO ()
getRecursiveContents ochan topdir = do
    writeChan ochan (Left True)
    _t <- forkIO $ grc (1+length topdir) topdir ochan
    return ()
  where
    grc n topdir ochan = do
      de <- doesDirectoryExist topdir
      when de $ do
              names <- {- traceShow topdir $ -} getDirectoryContents topdir
              let properNames = filter (`notElem` [".", ".."]) names
                  unthrottled name = do
                     let path = topdir </> name
                     -- putStrLn path
                     isDirectory <- doesDirectoryExist path
                     if isDirectory
                        then writeChan ochan (Left True) >> grc n path ochan
                        else catch (getFileSize path >>= \x -> writeChan ochan (Right ((drop n path), x))) ((const (return ()))::SomeException -> IO ())
               in mapM_ unthrottled properNames
      writeChan ochan (Left False)

simpleFind paths = do
  j <- newChan 
  mapM_ (getRecursiveContents j) paths
  -- let z = filter p names
  getAllChanContents j

main = do
  aa <- getArgs
  if null aa then  putStrLn "missing argument: specify directory" >> exitFailure
  else mapM_ print =<< simpleFind aa

