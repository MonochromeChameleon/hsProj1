module UserInteraction where

prompt :: String -> IO String
prompt str = do
    input <- inlinePrompt str 
    putStrLn ""
    return input

inlinePrompt :: String -> IO String
inlinePrompt str = do
    putStr $ str ++ " ~> "
    input <- getLine
    return input