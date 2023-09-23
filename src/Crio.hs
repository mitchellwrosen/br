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

-- | A @Crio r a@ computation has implicit access to an @r@, can perform @IO@, and returns an @a@; it is like
-- @"ReaderT-over-IO"@ as popularized by the @rio@ package.
--
-- Additionally, a @Crio r a@ computation supports short-circuiting a computation with a value that is tracked in the
-- type system (unlike 'throwIO').
--
-- This allows one to write programs that have multiple logical exit points (which may or may not correspond to
-- "exceptional" circumstances). The primary benefit of this style is readability. Such programs need not choose one of
-- three bad options:
--
--   1. Use the @ExceptT@ monad transformer for short-circuiting syntax, and decorate the program with @ExceptT@,
--      @runExceptT@, and @lift@s as necessary.
--   2. Incur a syntactic indentation via a pattern match or if-then-else any time an exit point is reached.
--   3. Throw an exception with 'throwIO', and remember to catch it where appropriate.
--
-- For example, consider the following program that returns the temperature in degrees Celsius. First, it consults an
-- accurate (but flakey) weather service. If that fails, it tries a less-accurate fallback. If that fails, it returns 0.
--
-- Here are a couple utility helpers we'll use.
--
-- @
-- onRightM :: Monad m => (b -> m a) -> m (Either a b) -> m a
-- swapEither :: Either a b -> Either b a
-- @
--
-- And here are the two temperature-fetching actions.
--
-- @
-- getAccurateTemperature     :: MonadIO m => m (Either String Int)
-- getLessAccurateTemperature :: MonadIO m => m (Either String Int)
-- @
--
-- First, we'll try using @ExceptT@.
--
-- @
-- whatsTheTemperature :: IO Int
-- whatsTheTemperature =
--   teardown do
--     output "I'm getting the temperature."
--     _ \<- ExceptT (swapEitherM \<\$> getAccurateTemperature)
--     output "That didn't work, trying one more time."
--     _ \<- ExceptT (swapEitherM \<\$> getLessAccurateTemperature)
--     output "That didn't work either!"
--     pure 0
--   where
--     teardown :: ExceptT Int IO Int -> IO Int
--     teardown action = do
--       temp \<- either id id \<\$> runExceptT action
--       output "All done!"
--       pure temp
--
-- @
--
-- Next, let's try pattern matching.
--
-- @
-- whatsTheTemperature :: IO Int
-- whatsTheTemperature = do
--   output "I'm getting the temperature."
--   temp \<-
--     getAccurateTemperature >>= \\case
--       Right temp -> pure temp
--       Left _ -> do
--         output "That didn't work, trying one more time."
--         getLessAccurateTemperature >>= \\case
--           Right temp -> pure temp
--           Left _ -> do
--             output "That didn't work either!"
--             pure 0
--   output "All done!"
--   pure temp
-- @
--
-- Next, let's try using an exception.
--
-- @
-- whatsTheTemperature :: IO Int
-- whatsTheTemperature =
--   teardown do
--     output "I'm getting the temperature."
--     _ \<- getAccurateTemperature & onRightM done
--     output "That didn't work, trying one more time."
--     _ \<- getLessAccurateTemperature & onRightM done
--     output "That didn't work either!"
--     pure 0
--   where
--     done :: Int -> IO void
--     done = throwIO . GotTheTemperature
--
--     teardown :: IO Int -> IO Int
--     teardown action = do
--       temp \<- catch action \\(GotTheTemperature temp) -> pure temp
--       output "All done!"
--       pure temp
--
-- newtype GotTheTemperature = GotTheTemperature Int
--   deriving stock (Show)
--   deriving anyclass (Exception)
-- @
--
--
-- And finally, we'll use @crio@.
--
-- @
-- whatsTheTemperature :: Crio IO Int
-- whatsTheTemperature = do
--   temp \<-
--     Crio.'label' \\done -> do
--       output "I'm getting the temperature."
--       getAccurateTemperature & onRightM (Crio.'goto' done)
--       output "That didn't work, trying one more time."
--       getLessAccurateTemperature & onRightM (Crio.'goto' done)
--       output "That didn't work either!"
--       pure 0
--   output "All done!"
--   pure temp
-- @
--
-- As you can see, the program grows looks similar to the version that uses exceptions, but it doesn't work outside of
-- the type system, and it's not possible to accidentally "leak" an exception that was meant to be caught.
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
