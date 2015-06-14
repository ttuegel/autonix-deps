module Autonix.Args where

import Control.Monad (join)
import Control.Monad.IO.Class
import Options.Applicative

withArgs :: MonadIO m => (FilePath -> m a) -> m a
withArgs child =
    join $ liftIO $ execParser $ info (helper <*> parser) fullDesc
  where
    parser = child <$> strArgument (metavar "MANIFEST")
