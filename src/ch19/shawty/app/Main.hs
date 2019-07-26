module Main where

{-# LANGUAGE OverloadedStrings #-}
-- OverloadedStrings makes Strings polymorphic over the Num type class
-- Strings are usually a concrete type, not polymorphic
-- OverloadedStrings allows Strings to be used as Text and ByteString values

-- import Lib
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

main :: IO ()
main = undefined
-- main = someFunc

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

-- choose a random element in the alphaNum range
randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- "right of arrow is IO Int,
  -- so randomDigit is Int"
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)
  
-- apply randomElement to alphaNum to get one random letter or number
shortyGen :: IO [Char]
shortyGen =
  replicateM 7 (randomElement alphaNum)

-- Arguments:
-- 1) connection to Redis (R.Connection)
-- 2) the key we are setting in Redis
-- 2) value the key gets set to
saveURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

-- takes a connection to Redis and the shortened URI 
-- gets the URI associated with the short URL to show users where they are going
getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply
              (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

-- returns output to the web browser  
linkShorty :: String -> String
linkShorty shorty =
  concat
  [ "<a href=\""
  , shorty
  , "\">Copy and paste your short URL</a>"
  ]

-- concatenating lists of Text values to give responses to the browser
-- since the final output to scotty has to be a Text value
-- TL.concat :: [TL.Text] -> TL.Text
shortyCreated :: Show a
              => a
              -> String
              -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            -- , " shorty is: "
            , TL.pack " shorty is: " -- not sure why I need to use pack if I have the OverloadedStrings pragma
            , TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat
    [ uri
    , TL.pack " wasn't a url,"
    , TL.pack " did you forget http://?"
    ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat
    [ TL.pack "<a href=\""
    , tbs, TL.pack "\">"
    , tbs, TL.pack "</a>" ]

app :: R.Connection
    -> ScottyM ()
-- redis connection that ought to have started before the web server started
app rConn = do
-- get takes a RoutePattern,"an action that returns a HTTP response and adds the route to the
-- Scotty server it is embedded in"
-- Since RoutePattern has an IsString instance, the argument can be a String literal
-- "/" is the to-level route (e.g. going to https://google.com/ or https://bitemyapp.com/)
-- "the last "/" is what is being expressed"
  get "/" $ do
    uri <- param "uri"

-- param gets parameters
-- like Read, but parsing a value of the type that you request
-- "param function can find arguments from URL path captures (see below with :short)"
-- first argument: key for the input
-- param :: Parsable a
--       => Data.Text.Internal.Lazy.Text
--       -> ActionM a
    let parsedUri :: Maybe URI
        parsedUri =
          parseURI (TL.unpack uri)

      -- test whether the user provided a valid URI through the use of the network-uri library's
      -- parseURI function
    case parsedUri of
      -- don't care about the Maybe datatype it got wrapped in
      Just _ -> do
        shawty <- liftIO shortyGen
        -- "the Monad here is ActionM (an alias of ActionT)"
        -- it is a datatype that represents code that handles web requests and returns responses
        -- Although IO actions can be performed in this Monad, one would need to lift the IO
        -- action over the additional structure
        -- MonadIO can auto-lift IO actions, but it can also be done manually
        let shorty = BC.pack shawty
        -- converts the short code for the URI into a Char8 ByteString for storage in Redis
            uri' =
              -- "converts the URI the user gave from a lazy Text value to a strict Text value
              -- then encodes as UTF-8 (popular Unicode format) ByteString for storage in Redis"
              encodeUtf8 (TL.toStrict uri)

        resp <-
          -- liftIO performs an IO action inside a scotty ActionM 
          -- saving the short code and the URI in Redis so that we can look for things using the
          -- short code as a key, then get back a URI "as a value as if it had been stored in the past"
          liftIO (saveURI rConn shorty uri')


        -- response that is returned when we successfully saved the short code for the URI
        -- provides a shortened URI for the user to share
        html (shortyCreated resp shawty)

      -- Error response in the event hat user provides a URI that wasn't valid
      Nothing -> text (shortyAintUri uri)
  -- "second handler handles requests to a shortened URI and returns the unshortened URL to follow:"

  -- URL path capture, such as requesting /foo from the server will make it get the key "foo" from Redis
  -- if there's a value store d in that key, return the URI in the response
  get ":/short" $ do
    -- same parameter as before, but this time we expect it to be a part of the path capture
    -- instead of a query argument
    short <- param "short"
    -- "lifting an IO action inside ActionM, this time to get the short code as the lookup key from
    -- Redis"
    uri <- liftIO (getURI rConn short)

    case uri of
      Left reply -> -- error
        -- Return a Text with the error so that the user knows what went wrong
        -- Redis has Showable errors
        text (TL.pack (show reply))

      -- a path
      Right mbBS -> case mbBS of
        -- key wasn't in the database (but no error)
        Nothing -> text "uri not found"

        -- Retrieve the key that is in the database, extract the ByteString out of the Just data
        -- constructor and "render the URI in a success template to show the user the URI we stored
        Just bs -> html (shortyFound tbs)
          -- "going in the opposite direction that we went in before"
          -- decode the ByteString assuming that it is encoded as UTF-8 and then convert the strict
          -- Text value into a lazy Text value
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)
     

-- "acts as an entry point for our web server when we start the executable"
-- first we invoke scotty 3000, a helper function from the scotty library that, given a port to run
-- on and a scotty application, listens for requests and responds to them
main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
  
