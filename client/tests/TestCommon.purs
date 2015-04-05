module TestCommon (runTestSuite, TestSuite(Suite)) where


import qualified Debug.Trace as Trace

import qualified Data.String as String
import Test.Unit


data TestSuite e = Suite {
  name :: String,
  test :: Test e
}


runTestSuite (Suite { name: name, test: test }) = do
  printTitle name
  runTest test


printTitle name = Trace.trace formattedTitle where

  formattedTitle = border ++ "\n" ++ center ++ "\n" ++ border where
    border = repeat "#" testTitleSize 
    center =
      let nameLength = String.length name 
          marginSize = testTitleSize - nameLength  
          lrMargin   = (repeat "" marginSize)
      in (lrMargin ++ name ++ lrMargin) 

  repeat s n = repeatIter n "" where
    repeatIter n acc 
      | n <= 0    = acc
      | otherwise = repeatIter (n-1) (s ++ acc)    

  testTitleSize = 80

