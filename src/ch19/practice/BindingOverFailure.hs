module BindingOverFailure where

-- binding over failure in initialisation

main :: IO ()
main = do
  initAndFp <- runEitherT $ do
    fp <- tryHead NoConfig =<< lift getArgs
    initCfg <- load' fp
    return (initCfg, fp)

    either bail (uncurry boot) initAndFp
    where
      boot initCfg fp =
        void $ runMVC empty
               oracleModel (core initCfg fp)

      bail NoConfig =
        errorExit "Please pass a config"

      bail (InvalidConfig e) =
        errorExit
          ("Invalid config " ++ show e)

      load' fp =
        hoistEither
        . fmapL InvalidConfig
          =<< (load fp)
