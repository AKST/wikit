module Data.Message where 

import Data.Status

newtype Message = Message { text :: String, status :: Status } 

