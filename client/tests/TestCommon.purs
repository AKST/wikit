module TestCommon where

import Control.Monad.Error.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Eff

import qualified Debug.Trace as Trace
import qualified Data.Either as Either
import qualified Data.String as String
import qualified Data.Argonaut as Argonaut
import Data.Argonaut.Decode (DecodeJson)



-- assertIsRight :: forall a e. Either.Either String a -> Assertion e
-- assertIsRight (Either.Left message) = assert message false  
-- assertIsRight (Either.Right _)      = return unit
-- 
-- 
-- parse string = do
--   let result = Argonaut.jsonParser string >>= Argonaut.decodeJson 
--   assertIsRight result
--   --
--   -- This will never be left
--   --
--   case result of
--     Either.Right r -> pure r

printTitle name = Trace.trace formattedTitle where

  formattedTitle = border ++ "\n" ++ center ++ "\n" ++ border where
    border = repeat "#" testTitleSize 
    center =
      let nameLength = String.length name 
          marginSize = testTitleSize - nameLength  
          lrMargin   = (repeat " " (marginSize / 2))
      in (lrMargin ++ name) 

  repeat s n = repeatIter n "" where
    repeatIter n acc 
      | n <= 0    = acc
      | otherwise = repeatIter (n-1) (s ++ acc)    

  testTitleSize = 80

