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
testParseSpaces = (fromJust . parse) " ( atom1  atom2  )  "
                  @?= (Sexp [Leaf $ Symbol "atom1",
                             Leaf $ Symbol "atom2"])

testParseNewlines :: Assertion
testParseNewlines = (fromJust . parse) " \n (\n atom1 \n atom2 \n ) \n\n"
                    @?= (Sexp [Leaf $ Symbol "atom1",
                               Leaf $ Symbol "atom2"])

-- Should be able to parse the "unparse" of every possible AST
propParseUnparse :: AST -> Bool
propParseUnparse ast = (fromJust . parse . unparse) ast == ast

main :: IO ()
main = defaultMainWithOpts [ testCase "unparse" testUnparse
                           , testCase "parseSpaces" testParseSpaces
                           , testCase "parseNewlines" testParseNewlines
                           , testProperty "parseUnparse" propParseUnparse] mempty
