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

Or, /Eff-able/ as in /able to be effected/ or /effectuated/ - something with the potential to become effects.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Effable
(
  -- * Type
  Effable
, Wrap

  -- * Create
, singleton
, embed
, string

-- * Transform
-- ** Items
, mapItems
, mapMaybe
-- ** Wraps
-- $wrap
, wrap
, wrapInside
, wrapEach
-- $wrap-overview

-- * Branching #branching#
, when'
, whenA
, onlyIf
, ifThenElse
, Enumerable
, byAction
, byActionMaybe
, embedAction
-- $branching-overview

-- * Effectuate
, run
, RunWith
, runWith

-- * Usage example
-- $env_example

)
where

import Data.String
import Data.Coerce
import Control.Monad
import Control.Applicative
import Data.Word (Word8)
import Data.Foldable
import Data.Maybe         qualified as Maybe
import System.Environment qualified as Env
import Control.Exception  qualified as E


{- implementation notes:

- user-facing terminology for one element of [Part m b]:
  - module docstring:
    - "a @b@", "each @b@" etc.; prefer over "item" to the extent reasonable
  - everywhere else (including later docstrings, identifier names):
    - "item(s)" (clarify with "a @b@" etc. where suitable, e.g. at first use after the module docstring and in 'run' docstring)

- instances of "\ \" etc. in docstrings are used to sync vertical alignment/whitespace padding between code and the generated haddock

- "> >>> ..." in docstrings can be used to inhibit doctest execution of the line yet have haddock render it as ">>> ..."

- haddock table bug: won't render if following immediately after heading
  - workaround: put a `&#32;` (= SGML whitespace) between

-}

{- $setup
>>> import Data.Word (Word8)
>>> import Control.Monad
>>> import Data.Foldable
>>> :set -XOverloadedStrings

Overrides for env_example:
>>> isShellM   = pure True  :: IO Bool
>>> isVerboseM = pure False :: IO Bool
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

-- single-element 'Foldable' and 'Traversable':
instance Foldable    (Part m) where foldMap  f (Part _ x) = f x
instance Traversable (Part m) where traverse f (Part w x) = Part w <$> f x

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

@
'pure' x  ==  'embed' x
@

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

effify :: ([Part m b]->[Part m b']) -> (Effable m b->Effable m b')
effify = coerce

wrapEm' :: (Wrap m -> Wrap m) -> Effable m b -> Effable m b
wrapEm' f = effify $ map (mapPartWrap f)

{-# INLINE wrapEm' #-}

{- | traverse methods: non-exported internal helpers

Currently unused.

May not be exported: they allow expressing folding over bare @b@s, which does not give a semantically meaningful result/violates opacity.
-}
_traverseEff :: (Applicative f) => (a -> f b) -> Effable m a -> f (Effable m b)
_traverseEff f (Effable ps) = Effable <$> traverse (traverse f) ps

_sequenceEff :: (Applicative f) => Effable m (f a) -> f (Effable m a)
_sequenceEff = _traverseEff id


--- create
--- ------

singleton :: Wrap m -> b -> Effable m b
singleton w l = Effable [Part w l]

embed :: b -> Effable m b
embed = singleton id

string :: IsString b => String -> Effable m b
string = embed . fromString


--- transform
--- ---------

{- | Map items (= map over @b@s).

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

{- | Map items for which the functional argument returns a 'Just' value; otherwise, don't include the item in the result.

@
'mapMaybe' f eff = eff >>= f'
  where
    f' x|Just y <- f x  =  'embed' y
        |otherwise      =  'mempty'
@

For filtering, creating the following function can be helpful:

@
filter' :: (b' -> Bool) -> 'Effable' f b' -> 'Effable' f b'
filter' p = 'mapMaybe' (\x -> if p x then Just x else Nothing)

-- or, equivalently, since 'Effable' has a 'MonadPlus' instance:
filter' = 'mfilter'
@

-}
mapMaybe :: (b -> Maybe b') -> Effable m b -> Effable m b'
mapMaybe f = effify (traverseJusts f)
  where
    {- For each element of the list, apply the function to each inner element and sequence the Maybe effect. Return a list of the Just results.

    > >>> traverseJusts (\x->if even x then Just (x `div` 2) else Nothing) [[2,4],[21,40],[60,80]]
    > [[1,2],[30,40]]

    -}
    traverseJusts :: Traversable t => (bb -> Maybe bb') -> [t bb] -> [t bb']
    traverseJusts f' = Maybe.mapMaybe (traverse f')


{- $wrap

=== Laws

These hold for both 'wrap' and 'wrapInside':

@
'wrap' f 'mempty'    ==  'mempty'
'wrap' f (x '<>' y)  ==  'wrap' f x '<>' 'wrap' f y   -- distributes over '<>'
g '<$>' 'wrap' f x   ==  'wrap' f (g '<$>' x) \ \     -- commutes with 'fmap'
@
-}

{- | Add an additional emission wrapper to all items.

The given function /composes outside of/ any existing wrappers.

@
'wrap' 'id'          ==  'id'
'wrap' (f . g)\ \    ==  'wrap' f . 'wrap' g
          \ \ \ \        -- composes /co/variantly

run' ('wrap' f x)\ \ ==  f '<$>' (run' x)      where run' = 'runWith' emit
@
-}
wrap :: Wrap m -> Effable m b -> Effable m b

{- | Add an additional emission wrapper to all items.

The given function /composes inside of/ any existing wrappers, i.e. it will be applied directly to the action produced by the emission.

@
'wrapInside' 'id'              ==  'id'
'wrapInside' (f . g)\ \        ==  'wrapInside' g . 'wrapInside' f
                \ \ \ \            -- composes /contra/variantly

'run' emit ('wrapInside' f x)  ==  'run' (f . emit) x
@
-}
wrapInside :: Wrap m -> Effable m b -> Effable m b

wrap       f x = wrapEm' (f .) x
wrapInside f x = wrapEm' (. f) x

{- | Add an additional emission wrapper to each item.

The given function /composes outside of/ any existing wrappers.

=== __For a @wrapEachInside@ function__

For a variant of this function that composes the new wrapper /inside of/ existing wrappers, the user may choose to define::

@
wrapEachInside :: (b -> 'Wrap' m) -> 'Effable' m b -> 'Effable' m b
wrapEachInside f eff =
  let g x = 'singleton' (f x) x
  in  eff '>>=' g
@

-}
wrapEach :: (b -> Wrap m) -> Effable m b -> Effable m b
wrapEach f = effify $ \ps -> [Part (f x . w  ) x | Part w x <- ps]

_wrapEachInside :: (b -> Wrap m) -> Effable m b -> Effable m b
_wrapEachInside f eff =
  let  g x = singleton (f x) x
  in   eff >>= g

{- $wrap-overview

== Wrap: overview

&#32;

+--------+---------------------------+---------------------------------------------------+
|        |Composition order          |Signature                                          |
+        +----------+----------------+                                                   |
|        |outside   |inside          |                                                   |
|        |          |                |                                                   |
+========+==========+================+===================================================+
|Uniform |'wrap'    |'wrapInside'    |@'Wrap' m        -> 'Effable' m b -> 'Effable' m b@|
+--------+----------+----------------+---------------------------------------------------+
|Per-item|'wrapEach'| wrapEachInside |@(b -> 'Wrap' m) -> 'Effable' m b -> 'Effable' m b@|
+--------+----------+----------------+---------------------------------------------------+

-}


--- branching
--- ---------

{- | /Conditional inclusion/.

Suppress the effects of emitting the value if an effectful predicate evaluates to False:

@
'run' emit ('when'' (pure False) x)  ==  pure ()
'run' emit ('when'' (pure True ) x)  ==  'run' emit x
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

{- | /Conditional inclusion/ (pure predicate).

Note that @'whenA' False _@ is not generally the same as 'mempty': @'whenA' False x@ preserves the structure of @x@ even though its emission will be suppressed:

>>> runEmit eff = putStr "result: " >> run putStr eff
>>> prepend_a = (putStr "a" *>)
>>> runEmit $ wrap prepend_a (whenA False (string "suppressed"))
result: a

>>> runEmit $ wrap prepend_a mempty
result:

-}
whenA
  :: Applicative m
  => Bool
  -> Effable m b -- ^ to include if True (else nothing)
  -> Effable m b
whenA b = wrap (when b)


{- | Flipped 'when''.

The high precedence allows expressions such as e.g.:

@
{-# LANGUAGE OverloadedStrings #-}

debugMsg =
 \ \    ("host: "'<>'host)  \`'onlyIf'\`  isPrintHostM
  '<>'   "is connected"\ \  \`'onlyIf'\`  isPrintStatusM
@

-}
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

In light of 'byAction', 'ifThenElse' can be viewed as 'byAction' specialized to a domain with the two inhabitants 'True' and 'False':

@
'ifThenElse' bM x y  ==  'byAction' bM (\\b -> if b then x else y)
@

(Between the LHS and RHS the ordering of the internal representation will be different; that is however not observable with read-like actions.)

=== Comparing the signature with '>>='

@
'byAction' :: ('Enumerable' a, eff b ~ t'Effable' m b) =>
\ \           'Monad' m => m a -> (a -> eff b) -> eff b
('>>=')    :: 'Monad' m => m a -> (a -> m   b) -> m   b
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
> λ> :set +s
>
> λ> run print effable
> True
> (ran for 0.01 secs, allocated 1,014,600 bytes)
>

An 'Int' is 'Enumerable' but has many inhabitants so reifying @IO Int@ becomes costly.

> --- GHCi session ---
>
> λ> ineffable = byAction (pure (1::Int)) embed
>
> λ> :set +s
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
  -- "why give the user a footgun, when you can give them a death star pointed right at their head?"
  -- a function like this shouldn't be seen anywhere near an API surface. But it is useful. So here it is. I'm not locking the door, but I _have_ put the tie on the doorknob; if you walk in, then what you see's on you.

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

{- | /Evaluation of finite-domain function/ whose result is a 'Maybe' value.

Using this function allows deciding to suppress evaluation results based on the value the action yielded. Yet, the internal representation only grows proportional to the number of domain inhabitants for which a 'Just' value is returned, rather than being proportional to the number of inhabitants.

@
'byAction' xM  f  ==  'byActionMaybe' xM ('Just' . f)
@
-}
byActionMaybe
  :: (Monad m, Enumerable a)
  => m a                         -- ^ monadic evaluation point
  -> (a -> Maybe (Effable m b))  -- ^ the function to evaluate
  -> Effable m b
byActionMaybe xM f = foldMapMaybe g domain
  where
    g d    = when' ((==d) <$> xM) <$> (f d)
    domain = [minBound..maxBound]

    foldMapMaybe :: (Foldable t, Monoid mm) => (x -> Maybe mm) -> t x -> mm
    foldMapMaybe f' = foldr comb mempty
      where
        comb x xs = case f' x of
          Just x' -> x' <> xs
          _       ->       xs

{- | /Evaluation of finite-domain value/ for the daring.

When emitted, the monadic action will be run once for each value of the domain.

@
'embedAction' x  ==  'byAction' x 'embed'
@

(The same warning as that for 'byAction' apply.)
-}
embedAction
  :: (Monad m, Enumerable a)
  => m a                     -- ^ monadic value
  -> Effable m a
embedAction xM = byAction xM embed

{- $branching-overview

== Branching: overview

&#32;

+---------------+------------------+--------------+--------------------------+
|               |Domain            |Representation|Range of emission         |
|               +------+-----------+∝ *1          |                          |
|               |      |\(\Sigma\) |              |                          |
|               |      |inhabitants|              |                          |
+===============+======+===========+==============+==========================+
|'when''        |'Bool'|2          |\(1\)         |x₁, @pure ()@             |
+---------------+------+-----------+--------------+--------------------------+
|'ifThenElse'   |'Bool'|2          |\(2\)         |x₁, x₂                    |
+---------------+------+-----------+--------------+--------------------------+
|'byAction'     |@a@   |/n/        |\(n\)         |x₁, ..., xₙ               |
+---------------+------+-----------+--------------+--------------------------+
|'byActionMaybe'|@a@   |/n/        |\(\leq n\)    |x₁, ..., xₘ, \(m \leq n\) |
+---------------+------+-----------+--------------+--------------------------+

__*1__: "the size of the internal representation is proportional to..."

-}


--- effectuate
--- ----------

{- | For each item (each @b@) of an t'Effable', emit it with the given function, then apply the composed emission wrapper associated with that item, and combine all results.

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
  => (b -> m ())      -- ^ emitting one item
  -> Effable m b
  -> m ()
run emit (Effable parts) = traverse_ (emitPart emit) parts

newtype RunWith a = RunWith [a]
  deriving (Functor, Foldable, Traversable)

{- | Create a representation of the individual emission results of an t'Effable'.

Methods of the 'Foldable' and 'Traversable' instances of the result type can be used e.g. to customize how the individual emission results are combined.

@
'Data.Foldable.sequenceA_' ('runWith' emit x)  ==  'run' emit x
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
  :: (b -> m ())      -- ^ emitting one item
  -> Effable m b
  -> RunWith (m ())
runWith emit (Effable parts) = coerce (emitPart emit <$> parts)


{-# INLINE singleton  #-}
{-# INLINE embed      #-}
{-# INLINE mapItems   #-}
{-# INLINE mapMaybe   #-}
{-# INLINE wrap       #-}
{-# INLINE wrapInside #-}
{-# INLINE when'      #-}
{-# INLINE whenA      #-}

{-# INLINEABLE run     #-}
{-# INLINEABLE runWith #-}


--- example
--- -------

-- note: for reasons of making tests deterministic, for some bindings, doctests will use hard-coded values from the $setup block

{- $env_example

The following creates an t'Effable' that

- represents a sequence of 'String's, and
- conditionally includes each 'String' based on an 'IO' action evaluated at emission time.

>>> import qualified System.Environment as Env
>>> import qualified Data.Maybe         as Maybe

> >>> isShellM   = Maybe.isJust <$> Env.lookupEnv "SHELL"       :: IO Bool
> >>> isVerboseM = Maybe.isJust <$> Env.lookupEnv "EFF_VERBOSE" :: IO Bool

>>> :set -XOverloadedStrings
>>> :{
linesEff :: Effable IO String
linesEff =
     "shell-invoked" `onlyIf` isShellM
  <> "verbose-info"  `onlyIf` isVerboseM
  <> "DONE."
:}

To emit the value, we use 'run' with 'putStrLn' as the emission function. The results shown below assume @SHELL@ is a set environment variable but
  @EFF_VERBOSE@ is not.

>>> :{
printEff :: Effable IO String -> IO ()
printEff = run putStrLn
:}

>>> printEff linesEff
shell-invoked
DONE.

The embedded 'String's can be 'fmap'-ed, e.g.:

>>> :{
leftline :: String -> String
leftline = ("| " <>)
:}

>>> printEff (leftline <$> linesEff)
| shell-invoked
| DONE.

It should be noted that the functionality we implemented above could just as well have been expressed without t'Effable': a value of type @'IO' ['String']@ can be equally capable. Its lines could be 'fmap'-ed with @leftline@, and including each list element (= line) could be conditioned on an @'IO' 'Bool'@.


=== Adding a wrapper

Let's say we have this function:

>>> import qualified Control.Exception as E
>>>
>>> :{
-- | Run an IO action and then print a line indicating
--   whether it threw an exception or not.
withReporting :: IO () -> IO ()
withReporting x = do
  r <- E.try x
  case (r :: Either E.SomeException ()) of
    Left  _ -> putStrLn "(FAIL)"
    Right _ -> putStrLn "(OK)"
:}

The following t'Effable' will apply @withReporting@ to any emission of the first 'String':

>>> :{
linesEff_reporting :: Effable IO String
linesEff_reporting =
     wrap withReporting ("shell-invoked" `onlyIf` isShellM  )
  <>                    ("verbose-info"  `onlyIf` isVerboseM)
  <> "DONE."
:}

E.g.:

>>> printEff linesEff_reporting
shell-invoked
(OK)
DONE.

>>> printEff (leftline <$> linesEff_reporting)
| shell-invoked
(OK)
| DONE.

The second output includes the line

> (OK)

rather than

> | (OK)

which is the intended behaviour:

- @leftline '<$>' ..@ modifies each embedded 'String' (= line), while
- @'wrap' withReporting@ modifies how each line is emitted.

The above example illustrates that an t'Effable' with an emission wrapper still retains transformability, e.g. with 'fmap'. If we would not use an t'Effable' but instead create an @'IO' _@ that includes the wrapper, it would have to be an @'IO' ()@ - which is opaque and no longer transformable.

-}

{- | Code used in the example. (internal/for maintenance + typechecking)

Useful terminal command:
```sh
(unset EFF_VERBOSE; SHELL=true cabal repl --repl-options="-e _env_example_code")
```
-}
_env_example_code :: IO ()
_env_example_code = printExpressions where

  -- Effable without wrapper
  -- -----------------------

  isShellM   = Maybe.isJust <$> Env.lookupEnv "SHELL"       :: IO Bool
  isVerboseM = Maybe.isJust <$> Env.lookupEnv "EFF_VERBOSE" :: IO Bool

  linesEff :: Effable IO String
  linesEff =
       "shell-invoked" `onlyIf` isShellM
    <> "verbose-info"  `onlyIf` isVerboseM
    <> "DONE."

  printEff :: Effable IO String -> IO ()
  printEff = run putStrLn

  leftline :: String -> String
  leftline = ("| " <>)

  _res_effWithout    = printEff linesEff
  _res_effWithout_ll = printEff (leftline <$> linesEff)


  -- Effable WITH wrapper
  -- --------------------

  withReporting :: IO () -> IO ()
  withReporting x = do
    r <- E.try x
    case (r :: Either E.SomeException ()) of
      Left  _ -> putStrLn "(FAIL)"
      Right _ -> putStrLn "(OK)"

  linesEff_reporting :: Effable IO String
  linesEff_reporting =
       wrap withReporting ("shell-invoked" `onlyIf` isShellM  )
    <>                    ("verbose-info"  `onlyIf` isVerboseM)
    <> "DONE."

  _res_eff_reporting    = printEff linesEff_reporting
  _res_eff_reporting_ll = printEff (leftline <$> linesEff_reporting)


  -- Implementations in IO (no Effable)
  -- ----------------------------------

  -- (keep this code even if not used in the docstring)

  linesIO :: IO [String]
  linesIO = do
    isShell   <- isShellM
    isVerbose <- isVerboseM
    pure $
         (if isShell   then ["shell-invoked"] else [])
      ++ (if isVerbose then ["verbose-info" ] else [])
      ++ ["DONE."]

  printIO     :: IO [String] -> IO ()
  printIO_ll  :: IO [String] -> IO ()
  printIO_ll' :: IO [String] -> IO ()

  printIO     x = x >>= traverse_ putStrLn
  printIO_ll  x = x >>= traverse_ (putStrLn . leftline) -- "modify emission"
  printIO_ll' x = printIO $ fmap leftline <$> x         -- "modify value"

  printIO__linesIO__reporting :: IO ()
  printIO__linesIO__reporting = do
    isShell   <- isShellM
    isVerbose <- isVerboseM
    withReporting $ when isShell   $ putStrLn "shell-invoked"
    id            $ when isVerbose $ putStrLn "verbose-info"
    putStrLn "DONE."


  -- (For observing what the expressions print)
  -- ------------------------------------------

  printExpressions :: IO ()
  printExpressions = do
    put "\n=== Effable without wrapper ==="
    put   "--- plain:"          ; printEff linesEff
    put   "--- leftlined:"      ; printEff (leftline <$> linesEff)

    put "\n=== Effable WITH wrapper ==="
    put   "--- plain:"          ; printEff linesEff_reporting
    put   "--- leftlined:"      ; printEff (leftline <$> linesEff_reporting)

    put "\n-----------------------------------"

    put "\n=== IO [String] without wrapper ==="
    put   "--- plain:"          ; printIO     linesIO
    put   "--- leftlined em.:"  ; printIO_ll  linesIO
    put   "--- leftlined value:"; printIO_ll' linesIO

    put "\n=== IO [String] WITH wrapper ==="
    put   "--- (plain, opaque):"; printIO__linesIO__reporting

    where
      put s = putStrLn ("\n" ++ s)
