module AxingTediousCode where

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

-- decoder that creates an object from a String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- query that runs against the DB and gets an array of Strings
fetchFn :: Query -> IO [String]

-- another "context initialiser" that also has IO
makeIoOnlyObj :: [SomeObj]
              -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
-- pipelineFn
--   :: Query
--   -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn query = do
--   a <- fetchFn query
--   case sequence (map decodeFn a) of
--     (Left err) -> return $ Left err
--     (Right res) -> do
--       a <- makeIoOnlyObj res
--       return $ Right a


-- after
-- pipelineFn
--   :: Query
--   -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn = do
--   a <- fetchFn query
--   traverse makeIoOnlyObj (mapM decodeFn a)

-- pointfree  
pipelineFn
  :: Query
  -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = do
  ((traverse makeIoOnlyObj
  -- . (mapM decodeFn) =<<) . fetchFn 
  . (traverse decodeFn) =<<) . fetchFn
