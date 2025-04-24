module Main (main) where
import MyParser
import System.Environment (getArgs)

main:: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do putStrLn $ "Command line arguments:\n  <yamlfile> - path to yaml file to convert."
        (arg:_) -> do content <- readFile arg
                      putStrLn $ yamlToJson content