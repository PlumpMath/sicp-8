module Scheme where
import Prelude hiding (exponent)
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as P
import Test.QuickCheck
import Control.Applicative
import Data.Char
import Control.Monad

data AST = Leaf Atom | Sexp [AST] | QuotedSexp [AST] deriving (Eq, Show)
data Atom = Float Double | Int Int | Symbol String | String String | Quoted String deriving (Eq, Show)

--instance Show AST where
--  show = unparse


instance Arbitrary AST where
  arbitrary = sized arbitraryAst
  shrink (Leaf _) = []
  shrink (Sexp asts) = asts ++ (Sexp <$> listsMissingOneElement asts)
  shrink (QuotedSexp asts) = asts ++ (QuotedSexp <$> listsMissingOneElement asts)

-- Get all possible lists that represent xs with one element removed
-- (not in the proper order, though).
listsMissingOneElement :: [a] -> [[a]]
listsMissingOneElement = go []
  where go _ [] = []
        go px (x:xs) = [px ++ xs] ++ (go (x:px) xs)

arbitraryAst :: Int -> Gen AST
arbitraryAst 0 = Leaf <$> arbitrary
arbitraryAst n = oneof [Leaf <$> arbitrary,
                        Sexp <$> arbitraryAstList (n - 1),
                        QuotedSexp <$> arbitraryAstList (n - 1)]

arbitraryAstList :: Int -> Gen [AST]
arbitraryAstList n = join (do
          NonNegative listSize <- arbitrary
          let m = n `div` listSize
          return $ replicateM listSize $ arbitraryAst m)

validSymbolStr :: Gen String
validSymbolStr = listOf1 arbitrary `suchThat` (\s -> not (length s == 0) && all (\c -> not (isSpace c || isDigit c || c == '(' || c == ')' || c == '\'' || c == '"')) s)

validStr :: Gen String
validStr = listOf arbitrary `suchThat` all (\c -> not (c == '"'))

instance Arbitrary Atom where
  arbitrary = oneof [Float <$> arbitrary,
                     Int <$> arbitrary,
                     Symbol <$> validSymbolStr,
                     String <$> validStr,
                     Quoted <$> validSymbolStr]

unparse :: AST -> String
unparse (Sexp asts) = "(" <> unwords (map unparse asts) <> ")"
unparse (QuotedSexp asts) = "'(" <> unwords (map unparse asts) <> ")"
unparse (Leaf atom) = case atom of
  Float f -> show f
  Int i -> show i
  Symbol s -> s
  String s -> "\"" <> s <> "\""
  Quoted s -> "'" <> s

parseSpaces :: P.Parser String
parseSpaces = many P.space

parseAST :: P.Parser AST
parseAST = parseSpaces *> (parseQuotedSexp <|> parseSexp <|> parseLeaf)

parseQuotedSexp :: P.Parser AST
parseQuotedSexp = do
  _ <- P.char '\''
  _ <- P.char '('
  asts <- parseAST `P.sepBy` P.space
  _ <- parseSpaces
  _ <- P.char ')'
  return $ QuotedSexp asts

parseSexp :: P.Parser AST
parseSexp = do
  _ <- P.char '('
  asts <- parseAST `P.sepBy` P.space
  _ <- parseSpaces
  _ <- P.char ')'
  return $ Sexp asts

parseLeaf :: P.Parser AST
parseLeaf = Leaf <$> (parseSpaces *> (parseFloat <|> parseInt <|> parseQuoted
                                      <|> parseString <|> parseSymbol))

parseInt :: P.Parser Atom
parseInt = do
  sign <- P.option '0' (P.char '-')
  decimal <- P.many1 P.digit
  let num = sign : decimal in
    return $ Int $ read num

parseFloat :: P.Parser Atom
parseFloat = do
  sign <- P.option '0' (P.char '-')
  predec <- many P.digit
  _ <- P.char '.'
  postdec <- P.many1 P.digit
  exponent <- P.option "e0" ((:) <$> P.char 'e' <*> ((:) <$> P.option '+' (P.char '-') <*> P.many1 P.digit))
  let num = sign : predec <> "." <> postdec <> exponent in
    return $ Float $ read num


parseSymbol :: P.Parser Atom
parseSymbol = do
  str <- P.takeWhile1 (P.notInClass "() ")
  return $ Symbol $ B.unpack str

parseString :: P.Parser Atom
parseString = do
  _ <- P.char '"'
  str <- P.takeTill (== '"')
  _ <- P.char '"'
  return $ String $ B.unpack str

parseQuoted :: P.Parser Atom
parseQuoted = do
  _ <- P.char '\''
  str <- P.takeTill (P.inClass "() ")
  return $ Quoted $ B.unpack str

parse :: String -> Either String AST
parse = P.parseOnly parseAST . B.pack

evaluate :: AST -> AST
evaluate = id
