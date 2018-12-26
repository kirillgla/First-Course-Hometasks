import           Test.Tasty

import           Evaluation
import           NameConversion

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ evaluationTests
    , nameConversionTests
    ]
