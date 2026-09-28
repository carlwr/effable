module TestHelpers.Classes
  ( functor
  , applicative
  , monad
  , monadApplicative
  ) where

import Hedgehog.Utils
import Hedgehog
import Test.Hspec
import Test.Hspec.Hedgehog
import Control.Monad (ap)


type Prop = PropertyT IO ()


functor ::
  ∀ f a aa aaa
  .  (Functor f, Eq (f a), Eq (f aaa), Show (f a), Show (f aaa))
  => (     aa -> aaa)
  -> (a -> aa       )
  -> PropertyT IO (f a)
  -> Spec
functor f g gen = describe "is Functor-lawful" $ do
  it' "Identity   " gen $ fmap id     ===.  id
  it' "Composition" gen $ fmap (f.g)  ===.  (fmap f . fmap g)


applicative :: ∀ f a b c d. (Applicative f, _)
  => (b -> c)
  -> PropertyT IO b      -- ^ a generator of values
  -> PropertyT IO (f a)  -- ^ a generator of 'Applicative's
  -> PropertyT IO (f (     b -> d))
  -> PropertyT IO (f (a -> b     ))
  -> Spec
applicative h gen gen_xs gen_fs gen_gs =
  describe "is Applicative-lawful" $ do
    it' "Identity    " gen_xs $ \xs -> (pure id <*> xs    )  ===  xs
    it' "Homomorphism" gen    $ \x  -> (pure h  <*> pure x)  ===  pure' (h x)

    it "Interchange" . hedgehog $ do
      x  <- gen
      fs <- gen_fs
      (fs <*> pure x) === (pure (\ff -> ff x) <*> fs)

    it "Composition" . hedgehog $ do
      fs <- gen_fs
      gs <- gen_gs
      xs <- gen_xs
      (fs <*> (gs <*> xs)) === (pure (.) <*> fs <*> gs <*> xs)

  where
    pure' = pure @f


monad
  :: (Monad m, _)
  => PropertyT IO a
  -> PropertyT IO (m a)
  -> PropertyT IO (m b)
  -> PropertyT IO (       b -> m c)
  -> PropertyT IO (a -> m b       )
  -> Spec
monad gen gen_xs gen_ys gen_f gen_g = do
  describe "is Monad-lawful" $ do
    it "Left identity" . hedgehog $ do
      x <- gen
      g <- gen_g
      (return x >>= g)  ===  g x

    it "Right identity" . hedgehog $ do
      ys <- gen_ys
      (ys >>= return)  ===  ys

    it "Associativity" . hedgehog $ do
      xs <- gen_xs
      f  <- gen_f
      g  <- gen_g
      (xs >>= (\y -> g y >>= f))  ===  ((xs >>= g) >>= f)


monadApplicative
  :: ∀ m a b. (Monad m, _)
  => PropertyT IO (m a)
  -> PropertyT IO (m (a -> b))
  -> Spec
monadApplicative gen_xs gen_fs  =
  describe "Applicative<->Monad agree" $
    it "(<*>) == ap" . hedgehog $ do
      fs <- gen_fs
      xs <- gen_xs
      (fs <*> xs)  ===  (fs `ap` xs)


it' :: String -> PropertyT IO a -> (a->Prop) -> Spec
it' desc gen f =
  it desc $ do
    xs <- gen
    f xs
