module Br
  ( M,
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

newtype M r a
  = M (forall x. r -> (a -> IO x) -> IO x)
  deriving stock (Functor)

instance Applicative (M r) where
  pure x = M \_ k -> k x
  (<*>) = Control.Monad.ap

instance Monad (M r) where
  return = pure
  M mx >>= f =
    M \r k ->
      mx r (\a -> unM (f a) r k)

instance MonadIO (M r) where
  liftIO m =
    M \_ k -> do
      x <- m
      k x

instance MonadReader r (M r) where
  ask = M \r k -> k r
  local f m = M \r -> unM m (f r)

unM :: M r a -> r -> (a -> IO x) -> IO x
unM (M k) =
  k

run :: r -> M r a -> IO a
run r m =
  unM m r pure

newtype Label a
  = Label (forall r void. a -> M r void)

instance Contravariant Label where
  contramap f (Label g) =
    Label (g . f)

label :: (Label a -> M r a) -> M r a
label f =
  M \r k -> do
    n <- newUnique
    Exception.try (run r (f (Label \x -> liftIO (throwIO (X n x))))) >>= \case
      Left err@(X m y)
        | n == m -> k (unsafeCoerce y)
        | otherwise -> throwIO err
      Right x -> k x

goto :: Label a -> a -> M r void
goto (Label f) x =
  f x

type Abort a =
  (?abort :: Label a)

-- | \"Stick\" a label @b@, making any @abort x@ call in the given argument equivalent to @goto b x@.
stick :: Label a -> (Abort a => b) -> b
stick g x = let ?abort = g in x

-- | Abort to the stuck label.
abort :: Abort a => a -> M r void
abort x =
  case ?abort of
    Label f -> f x

unabort :: (Abort e => M r a) -> M r (Either e a)
unabort action =
  label \done ->
    stick (contramap Left done) (Right <$> action)

data X = forall a. X Unique a

instance Exception X where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show X where
  show _ = "Br.X"

with :: (forall v. (a -> IO v) -> IO v) -> (a -> M r b) -> M r b
with f action =
  M \r k -> do
    b <- f (\a -> run r (action a))
    k b

with_ :: (forall v. IO v -> IO v) -> M r a -> M r a
with_ f action =
  M \r k -> do
    a <- f (run r action)
    k a

try :: Exception e => M r a -> M r (Either e a)
try (M action) =
  M \r k ->
    Exception.try (action r (k . Right)) >>= \case
      Left ex -> k (Left ex)
      Right x -> pure x
