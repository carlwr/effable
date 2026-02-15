{-| Description : A data structure for emission plans
    License     : MIT

An @t'Effable' m b@ is

- a __pure plan__ for the later emission of @b@s

- a representation of an __ordered sequence of @b@s__, each annotated with an /emission wrapper/ @m () -> m ()@

- __emitted__ to its eventual result @m ()@ __with 'run'__ (/interpretation/\//elimination/)

- fairly __opaque__

    - its constructor is not exported
    - observing it is only supported through 'run'/'runWith'

- __/niche: see Caveats section/__


== Why?

Compared to just working in the monadic @m b@ context, t'Effable' brings one particular distinguishing feature:

/An t'Effable' can undergo decoration with emission wrappers ('wrap', 'wrapInside') after which it is still a pure t'Effable'./

This means that even after having been modified with wrappers, or any other supported transformation...

- ...it is still a 'Functor', 'Applicative' and 'Monad' in @b@
- ...it can be transformed further and combined into more complex structures with '<>'

Emission wrappers can be applied to granular constituents of an t'Effable' as the user code is building it. The t'Effable' machinery will track the wrappers behind-the-scenes through all supported transformations so that the eventual emission respects them.


== Caveats and usage scope

- [Effectful predicates](#g:branching) and [emission wrappers](#v:wrap) typically run more than once when a plan is emitted.

    - these actions should yield the same value across evaluations, otherwise the inclusion of branches will be inconsistent
    - /therefore/, t'Effable' is only suitable when __these actions are read-like__ (return the same value over repeated evaluations and are free of externally observable side-effects)

- The nesting of combinators and use of '<*>' and '>>=' will grow the internal t'Effable' representation, and the number of times actions are run at emission, combinatorially.


== Intuition

/t'Effable's are kept pure through all supported transformations by representing all possible outcomes of actions that affect structure. Running the actions is deferred to emission time. This comes with the cost of the internal representation carrying a complexity proportional to all possible outcomes./

Metaphorically, t'Effable' is the many-worlds interpretation of /actions/ meaning actions can be represented without them interacting with the actual world (= purity) and its representation is not collapsed to an outcome until it is observed (= 'run').


== \"Effable\"?

/Effable/ as in /sayable/ or /utterable/.

Or, /Eff-able/ as in /able to be effected/ or /effectuated/ - something with the potential of becoming effects.
-}


module Effable
(
  -- * Type
  Effable
, Wrap

  -- * Create
, singleton
, embed
, string

-- * Transform items
, mapItems

-- * Transform wraps
-- $wrap
, wrap
, wrapInside

-- * Branching #branching#
, when'
, whenA
, onlyIf
, ifThenElse
, Enumerable
, byAction
, embedAction

-- * Effectuate
, run
, RunWith
, runWith

)
where

import Data.String (IsString (..))
import Data.Coerce
import Control.Monad (when, MonadPlus)
import Control.Applicative (Const(..), Alternative ((<|>), empty))
import Data.Word (Word8)
import Data.Foldable (traverse_)

{- implementation notes:

- instances of "\ \" etc. in docstrings are in order to sync vertical alignment/whitespace padding between code and the generated haddock

-}

{- $setup
>>> import Data.Word (Word8)
-}


--- types, type helpers
--- -------------------

-- | An emission wrapper.
type Wrap m = m () -> m ()


--- Part ---

data Part m b = Part
  { _partWrap  :: Wrap m
  , _partItem :: b
  }
  deriving (Functor)

instance Applicative (Part m) where
  pure :: b -> Part m b
  pure x = Part id x

  (<*>) :: Part m (b -> b') -> Part m b -> Part m b'
  Part wf lf <*> Part wx lx  =  Part (wf . wx) (lf lx)

mapPartWrap :: (Wrap m -> Wrap m) -> Part m b -> Part m b
mapPartWrap f (Part w l) = Part (f w) l

emitPart :: (b -> m ()) -> Part m b -> m ()
emitPart emit (Part w l) = w (emit l)

{-# INLINE mapPartWrap #-}
{-# INLINE emitPart    #-}


--- Effable ---

{- | An ordered sequence of values, each with an associated emission wrapper (default: 'id'). -}
newtype Effable m b = Effable { inEffable :: [Part m b] }
  deriving (Semigroup, Monoid, Functor)

instance (IsString b) => IsString (Effable m b) where
  fromString = string

{- | 'Applicative' models /list-like indeterminism/: the result of @xs '<*>' fs@ is /all combinations/ of embedded functions and embedded values.

In the result of @fs '<*>' xs@, emission wrappers of @f@s are composed on the outside of that of @xs@.

=== Examples

>>> import Data.Functor.Const (Const (..))
>>> runConst = run (\b -> Const [b])
>>> runConst $ (embed pred <> embed succ) <*> (embed '1' <> embed 'b')
Const "0a2c"

>>> runConst $ embed succ <*> (embed (1::Word8) <> whenA True (embed 5))
Const [2,6]

-}
instance Applicative (Effable m) where
  pure :: b -> Effable m b
  pure x = Effable [pure x]

  (<*>) :: Effable m (b -> b') -> Effable m b -> Effable m b'
  Effable fs <*> Effable xs  =
    Effable
      [ f <*> x
        | f <- fs
        , x <- xs
      ]
    -- == the list Applicative lifted through 'Part'

_effable_applicative_doctest_typechecks :: a
_effable_applicative_doctest_typechecks = undefined
  where
    runConst = run (\b -> Const [b])

    _ex0 :: Const [Char] ()
    _ex0 = runConst $ (embed pred <> embed succ) <*> (embed '1' <> embed 'b')

    _ex1 :: Const [Word8] ()
    _ex1 = runConst $ embed succ <*> (embed (1::Word8) <> whenA True (embed 5))

{- | 'Monad' models /list-like indeterminism/.

The result of @xs '>>=' f@ is the concatenation of the results of applying @f@ to each value embedded in @xs@.

Note that the 'Monad' instance of t'Effable' is not related to the type parameter @m@ in a value of type @t'Effable' m b@ - that @m@ is a parameterization of the eventual effectful context (@m ()@) that will be used for emission. The 'Monad' instance of t'Effable', on the other hand, allows for and defines monadic computations on @t'Effable' m b@ values themselves.

-}
instance Monad (Effable m) where
  (>>=) :: Effable m b -> (b -> Effable m b') -> Effable m b'
  Effable xs >>= f  =
    Effable
      [ Part (wx . wf) lf
        | Part wx lx <- xs
        , Part wf lf <- f' lx
      ]
    where
      f' = inEffable . f

-- | @('<|>')  ==  ('<>')@.
instance Alternative (Effable m) where
  (<|>) = (<>)
  empty = mempty

-- | @'Control.Monad.mplus'  ==  ('<>')@.
instance MonadPlus (Effable m)

effify :: ([Part m b]->[Part m b]) -> (Effable m b->Effable m b)
effify = coerce

wrapEm' :: (Wrap m -> Wrap m) -> Effable m b -> Effable m b
wrapEm' f = effify $ map (mapPartWrap f)

{-# INLINE wrapEm' #-}


--- functions
--- ---------

singleton :: Wrap m -> b -> Effable m b
singleton w l = Effable [Part w l]

embed :: b -> Effable m b
embed = singleton id

string :: IsString b => String -> Effable m b
string = embed . fromString

{- | Map items.

@
'mapItems' == 'fmap'
@

Example:

>>> evenEffect x = if even x then Just () else Nothing
>>> run evenEffect ((+1) <$> embed 3)
Just ()

-}
mapItems :: (b->b') -> Effable m b -> Effable m b'
mapItems = fmap

{- $wrap

These hold for both 'wrap' and 'wrapInside':

@
'wrap' f 'mempty'    ==  'mempty'
'wrap' f (x '<>' y)  ==  'wrap' f x '<>' 'wrap' f y   -- distributes over '<>'
g '<$>' 'wrap' f x   ==  'wrap' f (g '<$>' x) \ \     -- commutes with 'fmap'
@
-}

{- | Add an additional emission wrapper to each @b@.

The given function /composes outside of/ any existing wrappers.

@
'wrap' 'id'          ==  'id'
'wrap' (f . g)\ \    ==  'wrap' f . 'wrap' g

run' ('wrap' f x)\ \ ==  f '<$>' (run' x)      where run' = 'runWith' emit
@
-}
wrap :: Wrap m -> Effable m b -> Effable m b

{- | Add an additional emission wrapper to each @b@.

The given function /composes inside of/ any existing wrappers.

@
'wrapInside' 'id'              ==  'id'
'wrapInside' (f . g)\ \        ==  'wrapInside' g . 'wrapInside' f

'run' emit ('wrapInside' f x)  ==  'run' (f . emit) x
@
-}
wrapInside :: Wrap m -> Effable m b -> Effable m b

wrap       f x = wrapEm' (f .) x
wrapInside f x = wrapEm' (. f) x

{- | /Conditional inclusion/.

Suppress the effects of emitting the value if an effectful predicate evaluates to False:

@
'run' emit ('when'' (pure False) x)  == pure ()
'run' emit ('when'' (pure True ) x)  == 'run' emit x
@

'when'' has the distributive and commutation properties as those of 'wrap'.

When emitted, the monadic action will be run once for each element of the internal representation.
-}
when'
  :: Monad m
  => m Bool
  -> Effable m b -- ^ to include if True (else nothing)
  -> Effable m b
when' bM =
  wrap $ \action -> do
    b <- bM
    when b action

whenA
  :: Applicative m
  => Bool
  -> Effable m b -- ^ to include if True (else nothing)
  -> Effable m b
whenA b = wrap (when b)

{- | Flipped 'when''. -}
onlyIf
  :: Monad m
  => Effable m b  -- ^ to include if True (else nothing)
  -> m Bool
  -> Effable m b
onlyIf = flip when'
infixl 7 `onlyIf`

{- | /Binary choice/.

When emitted, the monadic action will be run twice for each element of the internal representation.
-}
ifThenElse
  :: Monad m
  => m Bool
  -> Effable m b  -- ^ to include if True
  -> Effable m b  -- ^ to include if False
  -> Effable m b
ifThenElse pM true false =
     (when'        pM  true )
  <> (when' (not<$>pM) false)

{- | Constraining to types whose inhabitants can be enumerated with @['minBound'..'maxBound']@.

Such types are types with a known and finite set of inhabitants, given that their 'Enum' and 'Bounded' instances behave within established social norms.
-}
type Enumerable a = (Enum a, Bounded a, Eq a)

{- | /Evaluation of finite-domain function/ for the daring.

When emitted, the monadic action will be run once for each element, for each inhabitant of the 'Enumerable' type that the action yields.

=== Comparing with '>>='

The signature can be compared to that of /monadic bind/ ('>>='):

@
'byAction' :: ('Enumerable' a, e b ~ t'Effable' m b) =>
\ \           'Monad' m => m a -> (a -> e b) -> e b
('>>=')    :: 'Monad' m => m a -> (a -> m b) -> m b
@

A possible interpretation of the two signatures is:

- '>>=' returns a value-yielding action that may depend on what value some other action yields

- 'byAction' returns a pure t'Effable' that may depend on what value some action yields

=== __Warning:__ should only be used with types of a few inhabitants

All outcomes of the action are reified in the internal representation. With 'IO'-like monads, there is no short-circuiting; neither will we be saved by laziness - the full internal representation is likely to be forced at emission time.

'Bool' has two inhabitants so reifying @IO Bool@ is not expensive.

> --- GHCi session ---
>
> λ> effable = byAction (pure True) embed
>
> λ> -- make GHCi report evaluation time and allocated bytes:
>
> λ> :set +s
>
> λ> run print effable
> True
> (ran for 0.01 secs, allocated 1,014,600 bytes)
>

An 'Int' is 'Enumerable' but has many inhabitants so reifying @IO Int@ becomes costly in both time and allocations.

> --- GHCi session ---
>
> λ> ineffable = byAction (pure (1::Int)) embed
>
> λ> :set +s
>
> λ> run print ineffable
> 1
> (ran for 1.02 million years, allocated 3.06e10 gigabytes)
>

-}
byAction
  :: (Monad m, Enumerable a)
  => m a                     -- ^ monadic evaluation point
  -> (a -> Effable m b)      -- ^ the function to evaluate
  -> Effable m b
byAction xM f = foldMap g domain
  where
    g d    = when' ((==d) <$> xM) (f d)
    domain = [minBound..maxBound]
  -- why give the user a footgun, when you can give them a death star pointed right at their head?
  -- a function like this shouldn't be seen anywhere near an API surface. But it is useful. So here it is.
  -- ...we're adults after all; it's not my job to lock the door - I've put the tie on the doorknob, if you walk in, then what you see is on you.

_byAction_doctest_code :: Int -> IO ()
_byAction_doctest_code n_limit = _ineffable_res'
  where
    _effable        = byAction (pure True) embed
    _effable_res    = run print _effable

    _ineffable      = byAction (pure (1::Int)) embed
    _ineffable_res  = run print _ineffable

    _ineffable'     = effify (take n_limit) _ineffable
    _ineffable_res' = run print _ineffable'

    {- calcs for GHCi session transcript:

    λ> :set +s

    λ> _byAction_doctest_code (round 0.5e6)
    (0.87 secs, 829,006,864 bytes)

    λ> (fromIntegral (maxBound :: Int) :: Float) * 2 / 0.5e6 * 0.87 / (60*60*24*365)
    1017799.8

    λ> (fromIntegral (maxBound :: Int) :: Float) * 2 / 0.5e6 * 829e6 / 1e12
    3.05847e10
    -}

{- | /Evaluation of finite-domain value/ for the daring.

When emitted, the monadic action will be run once for each value of the domain.

(The same warning as that for 'byAction' applies.)
-}
embedAction
  :: (Monad m, Enumerable a)
  => m a                     -- ^ monadic value
  -> Effable m a
embedAction xM = byAction xM embed

{- | For each @b@ of an t'Effable', emit it with the given function, then apply the composed emission wrapper associated with that @b@, and combine all results.

== Laws

[Monoid homomorphism]:

    @
    'run' emit 'mempty'    ==  pure ()
    'run' emit (x '<>' y)  ==  'run' emit x '*>' 'run' emit y
    @

[Naturality]:

    @
    'run' emit (f '<$>' x)  ==  'run' (emit . f) x
    @

== Examples

These examples use @t'Const' []@ whose emission effect is to accumulate emitted items into a list.

>>> import Data.Functor.Const (Const (..))
>>> emitConst b = Const [b]

>>> run emitConst (embed 'a' <> embed 'b')
Const "ab"

With emission wrapper:

>>> silence = wrap (\_ -> Const [])
>>> run emitConst (embed 'a' <> (silence $ embed 'b') <> embed 'c')
Const "ac"

-}
run
  :: Applicative m
  => (b -> m ())      -- ^ emitting one @b@
  -> Effable m b
  -> m ()
run emit (Effable parts) = traverse_ (emitPart emit) parts

newtype RunWith a = RunWith [a]
  deriving (Functor, Foldable, Traversable)

{- | Create a representation of the individual emission results of an t'Effable'.

Methods of the 'Foldable' and 'Traversable' instances of the result type can be used e.g. to customize how the individual emission results are combined.

@
'Data.Foldable.sequenceA_' ('runWith' emit x)  == 'run' emit x
@

=== Example

The following uses the 'foldr' method of t'RunWith'\'s 'Foldable' instance to create a 'run'-like function with a 'foldr'-style API:

>>> :{
  let
    run_foldr
      :: (b -> m ())
      -> (m () -> c -> c)
      -> c
      -> Effable m b
      -> c
    run_foldr emit comb z = foldr comb z . runWith emit
:}

-}
runWith
  :: (b -> m ())      -- ^ emitting one @b@
  -> Effable m b
  -> RunWith (m ())
runWith emit (Effable parts) = coerce (emitPart emit <$> parts)


{-# INLINE singleton  #-}
{-# INLINE embed      #-}
{-# INLINE mapItems   #-}
{-# INLINE wrap       #-}
{-# INLINE wrapInside #-}
{-# INLINE when'      #-}
{-# INLINE whenA      #-}

{-# INLINEABLE run     #-}
{-# INLINEABLE runWith #-}
