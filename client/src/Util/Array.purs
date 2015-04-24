module Util.Array where 


import Data.Maybe
import qualified Data.Array as Array


type EndSplit a = { init :: [a], last :: a }  
type StartSplit a = { head :: a, tail :: [a] }  


splitEnd :: forall a. [a] -> Maybe (EndSplit a)
splitEnd list = do
  init <- Array.init list
  last <- Array.last list
  pure { init: init, last: last }


splitStart :: forall a. [a] -> Maybe (StartSplit a)
splitStart list = do
  head <- Array.head list
  tail <- Array.tail list
  pure { head: head, tail: tail }


any :: forall a. (a -> Boolean) -> [a] -> Boolean
any p [] = false
any p (x:xs) 
  | p x       = true
  | otherwise = any p xs


