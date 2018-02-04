module Parser(Expr(..), Atom(..), parseExprs, parse) where

import Control.Applicative((<$>))
import Data.Maybe(fromMaybe)
import Data.Char(isDigit)
import Numeric(readSigned, readHex, readOct, readInt)

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Combinator as P


data Expr = ExprList [Expr]
          | ExprAtom Atom
  deriving (Eq, Show)

data Atom = AtomSym String
          | AtomStr String
          | AtomInt Int
          | AtomKey String
  deriving (Eq, Show)


langDef :: P.LanguageDef st
langDef = P.LanguageDef
  { P.commentStart    = "#|"
  , P.commentEnd      = "|#"
  , P.commentLine     = ";"
  , P.nestedComments  = True
  , P.identStart      = P.alphaNum P.<|> P.oneOf "_.+-*/%=<>!?"
  , P.identLetter     = P.alphaNum P.<|> P.oneOf "_.+-*/%=<>!?"
  , P.opStart         = P.oneOf "&'#\\@"
  , P.opLetter        = P.oneOf "&'#\\@"
  , P.reservedNames   = []
  , P.reservedOpNames = ["'", "#'"]
  , P.caseSensitive   = True
  }


parseExprs = P.many parseExpr
parseExpr = P.whiteSpace lexer >> (parseExprList P.<|> parseExprAtom)
parseExprList = ExprList <$> P.parens lexer parseExprs
parseExprAtom = ExprAtom <$> (parseAtomStr P.<|> parseAtomSym P.<|> parseHex P.<|> parseOct P.<|> parseBin P.<?> "expression")

parseAtomStr = AtomStr <$> P.stringLiteral lexer

parseAtomSym = do
  s <- P.identifier lexer
  case tryReadSignedInt s of
    Just n  -> return $ AtomInt n
    Nothing -> return $ AtomSym s

lexer = P.makeTokenParser langDef

test = P.parseTest parseExprs "(1 2 (3))"

parse :: Maybe String -> String -> [Expr]
parse filename s = case P.parse parseExprs (fromMaybe "" filename) s of 
  Left   e -> error (show e)
  Right xs -> xs


tryReadSignedInt ('+':xs) = tryReadInt xs
tryReadSignedInt ('-':xs) = negate `fmap` tryReadInt xs
tryReadSignedInt xs       = tryReadInt xs
tryReadInt xs 
  | all isDigit xs  = Just ((read xs) :: Int)
  | otherwise       = Nothing

parseHex = P.try $ do
  P.char '#'
  P.oneOf "hHxX"
  sgn <- P.optionMaybe (P.oneOf "+-")
  hs <- P.many1 (P.oneOf "0123456789abcdefgABCDEFG")
  case sgn of
    Nothing -> return $ AtomInt $ fst $ head $ readHex hs
    Just s  -> return $ AtomInt $ fst $ head $ readSigned readHex (s:hs)

parseOct = P.try $ do
  P.char '#'
  P.oneOf "oO"
  sgn <- P.optionMaybe (P.oneOf "+-")
  hs <- P.many1 (P.oneOf "012345678")
  case sgn of
    Nothing -> return $ AtomInt $ fst $ head $ readOct hs
    Just s  -> return $ AtomInt $ fst $ head $ readSigned readOct (s:hs)

parseBin = P.try $ do
  P.char '#'
  P.oneOf "bB"
  sgn <- P.optionMaybe (P.oneOf "+-")
  hs <- P.many1 (P.oneOf "01")
  let readBin = readInt 2 (`elem` "01") (\s -> if s == '0' then 0 else 1) 
  case sgn of
    Nothing -> return $ AtomInt $ fst $ head $ readBin hs
    Just s  -> return $ AtomInt $ fst $ head $ readSigned readBin (s:hs)
