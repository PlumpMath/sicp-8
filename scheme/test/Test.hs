import Scheme
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Data.Maybe

testUnparse :: Assertion
testUnparse = unparse (Sexp [Leaf $ Symbol "atom"]) @?= "(atom)"

propParseUnparse :: AST -> Bool
propParseUnparse ast = (fromJust . parse . unparse) ast == ast

main :: IO ()
main = defaultMainWithOpts [ testCase "unparse" testUnparse
                           , testProperty "parseUnparse" propParseUnparse] mempty
