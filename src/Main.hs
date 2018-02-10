module Main where

import qualified Translate as T
import System.IO(readFile)
import System.Environment(getArgs)

main = do
  [f] <- getArgs
  src <- readFile f
  putStrLn "#include \"runtime.h\""
  putStrLn $ T.compileSource (Just f) src
  putStrLn "int main() { return m_main(); }"

