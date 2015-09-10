import Scheme
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Data.Maybe

testUnparse :: Assertion
testUnparse = unparse (Sexp [Leaf $ Symbol "atom"]) @?= "(atom)"

testParseSpaces :: Assertion
testParseSpaces = parse " ( atom1  atom2  )  "
                  @?= Right (Sexp [Leaf $ Symbol "atom1",
                                   Leaf $ Symbol "atom2"])

testParseNewlines :: Assertion
testParseNewlines = parse " \n (\n atom1 \n atom2 \n ) \n\n"
                    @?= Right (Sexp [Leaf $ Symbol "atom1",
                                     Leaf $ Symbol "atom2"])

-- Should be able to parse the "unparse" of every possible AST
propParseUnparse :: AST -> Bool
propParseUnparse ast = (parse . unparse) ast == Right ast

main :: IO ()
main = defaultMainWithOpts [ testCase "unparse" testUnparse
                           , testCase "parseSpaces" testParseSpaces
                           , testCase "parseNewlines" testParseNewlines
                           , testProperty "parseUnparse" propParseUnparse] mempty
