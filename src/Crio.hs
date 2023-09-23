module Crio
  ( Crio,
    run,

    -- ** Labels
    Label,
    label,
    goto,

    -- *** Implicit labels
    Abort,
    stick,
    abort,
    unabort,

    -- ** Acquiring resources
    with,
    with_,

    -- ** Catching IO exceptions
    try,
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, throwIO)
import Control.Exception qualified as Exception
import Control.Monad qualified
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Data.Functor.Contravariant
import Data.Unique
import Unsafe.Coerce (unsafeCoerce)

newtype Crio r a
  = Crio (forall x. r -> (a -> IO x) -> IO x)
  deriving stock (Functor)

instance Applicative (Crio r) where
  pure x = Crio \_ k -> k x
  (<*>) = Control.Monad.ap

instance Monad (Crio r) where
  return = pure
  Crio mx >>= f =
    Crio \r k ->
      mx r (\a -> unCrio (f a) r k)

instance MonadIO (Crio r) where
  liftIO m =
    Crio \_ k -> do
      x <- m
      k x

instance MonadReader r (Crio r) where
  ask = Crio \r k -> k r
  local f m = Crio \r -> unCrio m (f r)

unCrio :: Crio r a -> r -> (a -> IO x) -> IO x
unCrio (Crio k) =
  k

run :: r -> Crio r a -> IO a
run r m =
  unCrio m r pure

newtype Label a
  = Label (forall r void. a -> Crio r void)

instance Contravariant Label where
  contramap f (Label g) =
    Label (g . f)

label :: (Label a -> Crio r a) -> Crio r a
label f =
  Crio \r k -> do
    n <- newUnique
    Exception.try (run r (f (Label \x -> liftIO (throwIO (X n x))))) >>= \case
      Left err@(X m y)
        | n == m -> k (unsafeCoerce y)
        | otherwise -> throwIO err
      Right x -> k x

goto :: Label a -> a -> Crio r void
goto (Label f) x =
  f x

type Abort a =
  (?abort :: Label a)

-- | \"Stick\" a label @b@, making any @abort x@ call in the given argument equivalent to @goto b x@.
stick :: Label a -> ((Abort a) => b) -> b
stick g x = let ?abort = g in x

-- | Abort to the stuck label.
abort :: (Abort a) => a -> Crio r void
abort x =
  case ?abort of
    Label f -> f x

unabort :: ((Abort e) => Crio r a) -> Crio r (Either e a)
unabort action =
  label \done ->
    stick (contramap Left done) (Right <$> action)

data X = forall a. X Unique a

instance Exception X where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show X where
  show _ = "<<internal crio exception>>"

with :: (forall v. (a -> IO v) -> IO v) -> (a -> Crio r b) -> Crio r b
with f action =
  Crio \r k -> do
    b <- f (\a -> run r (action a))
    k b

with_ :: (forall v. IO v -> IO v) -> Crio r a -> Crio r a
with_ f action =
  Crio \r k -> do
    a <- f (run r action)
    k a

try :: (Exception e) => Crio r a -> Crio r (Either e a)
try (Crio action) =
  Crio \r k ->
    Exception.try (action r (k . Right)) >>= \case
      Left ex -> k (Left ex)
      Right x -> pure x
