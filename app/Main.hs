module Main (main) where
import MyParser
import System.Environment (getArgs)


main:: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do putStrLn $ "Command line arguments:\n  <yamlfile> - path to yaml file to convert."
        (inputf:outputf:_) -> do
            content <- readFile inputf
            writeFile outputf $ yamlToJson content
        (inputf:_) -> do
            content <- readFile inputf
            putStrLn $ yamlToJson content