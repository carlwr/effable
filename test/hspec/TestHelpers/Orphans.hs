{-# OPTIONS_GHC -Wno-orphans #-}

module TestHelpers.Orphans () where

import Data.Effable qualified as UUT
import Data.Effable (Effable, RunWith)
import Data.Foldable (toList)

--- instances

instance (Show a, m ~ ((,) [a])) => Show (Effable m a) where
  show = show . observe

instance (Eq a, m ~ ((,) [a])) => Eq (Effable m a) where
  x==y  =  observe x == observe y

instance Show a => Show (RunWith a) where
  show = show . toList

instance Eq a => Eq (RunWith a) where
  x==y  =  toList x == toList y


--- helpers

observe :: Effable ((,) [b]) b -> [[b]]
observe = fmap stripUnit . toList . UUT.runWith emit_tup
  where
    stripUnit (xs,()) = xs

emit_tup :: a -> ([a], ())
emit_tup x = ([x],())
