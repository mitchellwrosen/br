module Crio2
  ( -- * Labels
    Label,
    label,
    goto,

    -- ** Implicit labels
    Abort,
    stick,
    abort,
    unabort,
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, throwIO)
import Control.Exception qualified as Exception
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Contravariant
import Data.Unique
import Unsafe.Coerce (unsafeCoerce)

newtype Label a
  = Label (forall x. a -> IO x)

instance Contravariant Label where
  contramap f (Label g) =
    Label (g . f)

label :: (Label a -> IO a) -> IO a
label f = do
  n <- newUnique
  Exception.try (f (Label \x -> liftIO (throwIO (X n x)))) >>= \case
    Left err@(X m y)
      | n == m -> pure (unsafeCoerce y)
      | otherwise -> throwIO err
    Right x -> pure x

goto :: Label a -> a -> IO x
goto (Label f) x =
  f x

type Abort a =
  (?abort :: Label a)

-- | \"Stick\" a label @b@, making any @abort x@ call in the given argument equivalent to @goto b x@.
stick :: Label a -> ((Abort a) => b) -> b
stick g x = let ?abort = g in x

-- | Abort to the stuck label.
abort :: (Abort a) => a -> IO x
abort x =
  case ?abort of
    Label f -> f x

unabort :: ((Abort e) => IO a) -> IO (Either e a)
unabort action =
  label \done ->
    stick (contramap Left done) (Right <$> action)

data X = forall a. X Unique a

instance Exception X where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show X where
  show _ = "<<internal crio exception>>"
