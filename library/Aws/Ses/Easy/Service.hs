module Aws.Ses.Easy.Service
  ( Service(..)
  , sendEmail'
  ) where

import qualified Network.AWS as Aws
import qualified Network.AWS.SES as Aws
import Aws.Ses.Easy.Email (Email, fromEmail)
import Control.Exception.Lens (catching_)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (MonadCatch(..))


class Monad m => Service m where
  sendEmail :: Email -> m () -> m ()

sendEmail' :: (MonadIO m, MonadReader s m, MonadCatch m) => (s -> Aws.Env) -> Email -> m () -> m ()
sendEmail' getEnv email failed = do
  env <- asks getEnv
  let msg = fromEmail email
  _ <- catching_ Aws._MessageRejected (awsSendEmail env msg >> return ()) failed
  return ()
  where
    awsSendEmail env msg = liftIO $ Aws.runResourceT . Aws.runAWS env $ Aws.send msg
