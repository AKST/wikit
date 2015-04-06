module TestCommon where


import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))


testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)


