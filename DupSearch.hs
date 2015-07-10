{-
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension )
import System.Posix (getFileStatus, fileSize)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Concurrent.Chan
import Debug.Trace
-}
import Preface.R0ml
import Control.Concurrent.Async

getFileSize :: FilePath -> IO Integer
-- getFileSize path = getFileStatus path >>= return . fromIntegral . fileSize
getFileSize path = (fromIntegral . fileSize) <$> getFileStatus path

getRecursiveContents :: FilePath -> IO [(FilePath, Integer)]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
--  let throttled name = bracket_ (waitQSem qs) (signalQSem qs) $ do
  let unthrottled name = do
         let path = topdir </> name
         -- putStrLn path
         isDirectory <- doesDirectoryExist path
         if isDirectory
            then getRecursiveContents path
            else getFileSize path >>= \x -> return [(path, x)]
  as <- mapM (async . unthrottled) properNames
  rs <- mapM wait as
  return (concat rs)


simpleFind path = do
  names <- getRecursiveContents path
-- Good one for pointfree
  return (map (\(x,y) -> (drop (1+length path) x, y)) names)

extension c p = takeExtension p == '.':c 

main = do
  aa <- getArgs
  a <- case aa of 
    (aa:_) -> return aa
    otherwise -> putStrLn "missing argument: specify directory" >> exitFailure >> return "failed"

-- GOOD ONE!
  mapM_ (putStrLn . show) =<< simpleFind a

