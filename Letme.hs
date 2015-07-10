
import Preface.R0ml

main = do
  a <- getArgs
  let b = head a
      c = tail a

  
  print $ if null a then "" else b ++ " " ++ show (length c)  
  putStrLn "Good so far!"
  print b

