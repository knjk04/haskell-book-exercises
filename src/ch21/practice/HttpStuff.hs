module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]
       
mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

-- if we want one large IO action that returns a list of responses
-- insetad of a list of IO actions that we can perform to get a response
traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
