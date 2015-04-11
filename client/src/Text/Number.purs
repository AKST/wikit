module Text.Number where

import qualified Data.Enum as Enum
import qualified Data.String as String

prefixedZeros :: Number -> Number -> String  
prefixedZeros initZeros n = impl zeros strNumber where

  strNumber = show n

  zeros = initZeros - String.length strNumber 

  impl count acc
    | count <= 0 = acc
    | otherwise  = impl (count-1) ("0" ++ acc)


