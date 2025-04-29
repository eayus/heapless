module Util where

import Control.Monad.Except

-- These should be in base now?
tryError :: (MonadError e m) => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

withError :: (MonadError e m) => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure