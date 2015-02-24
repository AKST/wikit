module Wikipedia where


import Data.Word


-- http://en.wikipedia.org/w/api.php
--   ?action=query
--   &prop=revisions
--   &titles=Adolf%20Hitler
--   &rvprop=content|timestamp
--   &rvlimit=2
--   &format=JSON


data WikiRequest = WikiRequest


