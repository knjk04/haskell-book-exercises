module ConcatenatingConnectionParameters where

-- need to add imports
runDb :: SqlPersist (ResourceT IO) a
      -> IO a
runDb query = do
  let connStr =
        foldr (\(k,v) t ->
                t <> (encodeUtf8 $
                k <> "=" <> v <> " "))
        "" params
  runResourceT
    . withPostgresqlConn connStr
    $ runSqlConn query
