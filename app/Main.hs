module Main (main) where
import MyParser (yamlToJson)

main :: IO ()
main = do
  content <- readFile "input.yaml"
  putStrLn $ yamlToJson content