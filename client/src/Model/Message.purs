module Model.Message where 

import Model.Status

newtype Message = Message { text :: String, status :: Status } 

