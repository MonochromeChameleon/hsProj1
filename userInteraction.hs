module UserInteraction where

prompt :: String -> IO String
prompt str = do 
    putStr $ str ++ " ~> "
    input <- getLine
    putStrLn ""
    return input
