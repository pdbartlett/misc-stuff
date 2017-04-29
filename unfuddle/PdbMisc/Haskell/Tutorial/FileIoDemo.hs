module FileIoDemo where

main = doLoop

doLoop :: IO ()
doLoop = do
  putStrLn "'read', 'write' or 'quit'?"
  cmd <- getLine
  case cmd of
    "read"  -> do doRead
                  doLoop
    "write" -> do doWrite
                  doLoop
    "quit"  -> do putStrLn "Exiting..."
                  return ()
    _       -> do putStrLn "Huh?"
                  doLoop

doRead = do
  putStrLn "Enter file name:"
  filename <- getLine
  contents <- readFile filename
  putStrLn contents
  
doWrite = do
  putStrLn "Enter file name:"
  filename <- getLine
  putStrLn "Enter text (terminated by '.' on own line):"
  contents <- getText
  writeFile filename contents
 
getText :: IO String 
getText = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- getText
      return (line ++ "\n" ++ rest)
