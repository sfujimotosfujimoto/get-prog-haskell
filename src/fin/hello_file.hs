import System.IO 

main :: IO ()
main = do
  helloFile <- openFile "src/hello.txt" ReadMode
  hasLine <- hIsEOF helloFile 
  firstLine <- if not hasLine 
               then hGetLine helloFile
               else return "empty"
  putStrLn firstLine
  secondLine <- if not hasLine
                then hGetLine helloFile
                else return "empty"
  goodbyeFile <- openFile "src/goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine 
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"
  