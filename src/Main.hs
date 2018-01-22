module Main where

import qualified Syntax as S
import qualified Parser as P
import System.IO(readFile)
import System.Environment(getArgs)

main = do
  [f] <- getArgs
  src <- readFile f
  putStrLn "#include \"runtime.h\""
  putStrLn $ S.compileSource (Just f) src
  putStrLn "int main() { return m_main(); }"

