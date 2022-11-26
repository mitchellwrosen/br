module Br
  ( M,
    run,
    Goto,
    label,
    Abort,
    stick,
    abort,
    with,
    with_,
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, throwIO, try)
import Control.Monad qualified
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
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

type Goto a =
  forall r void. a -> M r void

label :: (Goto a -> M r a) -> M r a
label f =
  M \r k -> do
    n <- newUnique
    try (run r (f (\x -> liftIO (throwIO (X n x))))) >>= \case
      Left err@(X m y)
        | n == m -> k (unsafeCoerce y)
        | otherwise -> throwIO err
      Right x -> k x

type Abort a =
  (?abort :: Abort_ a)

data Abort_ a
  = Abort_ (Goto a)

stick :: Goto a -> (Abort a => b) -> b
stick f x = let ?abort = Abort_ f in x

abort :: Abort a => a -> M r void
abort x =
  case ?abort of
    Abort_ f -> f x

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
