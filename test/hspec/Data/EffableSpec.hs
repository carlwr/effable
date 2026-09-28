module Data.EffableSpec
( spec
) where

import Data.Effable qualified as UUT
import Data.Effable (Effable, Wrap)
import TestHelpers.Classes qualified as Classes
import TestHelpers.Orphans ()

import Test.Hspec hiding (it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Hedgehog
import Hedgehog.Range qualified as Range
import Hedgehog.Utils
import Hedgehog.Utils.Gen qualified as Gen

import Control.Applicative
import Control.Monad
import Data.String (IsString(fromString))
import Data.Foldable
import Data.List (intercalate)
import GHC.Stack (withFrozenCallStack)


spec :: Spec
spec = do

  when False $ do
    describe "generator: effables" spec_generator_eff
    describe "generator: wraps"    spec_generator_wrap

  describe "smoke"             spec_smoke
  describe "mapMaybe"          spec_mapMaybe
  describe "wrap*: laws"       spec_wraps_laws
  describe "wrap*: functions"  spec_wraps_functions
  describe "when*"             spec_when
  describe "byAction"          spec_byAction
  describe "byActionMaybe"     spec_byActionMaybe
  describe "run*"              spec_run

  describe "Effable instances" $ do
    spec_instances_laws
    spec_instances_wrapperComp


spec_smoke :: Spec
spec_smoke = do

  asUnitTests $
    describe "smoke tests" $ do
      it "agrees 3+1 is even" $ do
        let evenEffect x = if even x then Just () else Nothing
        UUT.run evenEffect ((+1) <$> UUT.embed 3) === Just ()
      it "Applicative" $ do
        let
          fs = UUT.embed pred <> UUT.embed succ
          xs = UUT.embed '1' <> UUT.embed 'b'
        UUT.run (\b -> Const [b]) (fs <*> xs) === Const "0a2c"


spec_mapMaybe :: Spec
spec_mapMaybe = do

  let
    f str|length str>=2  = Just str
         |otherwise      = Nothing

    f' x |Just y <- f x  =  UUT.embed y
         |otherwise      =  mempty

  it "law" $ do
    e <- forAllEffable genStr genWrap
    UUT.mapMaybe f e === (e >>= f')

  it "error injection fails the test (meta-test)" $
    assertFailing $
    do
      e <- forAllEffable genStr genWrap
      let f_err = f . reverse
      UUT.mapMaybe f_err e === (e >>= f')

  it "docstring's filter' == mfilter" $ do
    let
      uut_filter' p = UUT.mapMaybe (\x -> if p x then Just x else Nothing)
      f_p = even . length
    e <- forAllEffable genStr genWrap
    uut_filter' f_p e === mfilter f_p e


spec_wraps_laws :: Spec
spec_wraps_laws =
  for_
    [(UUT.wrap      ,"wrap"      )
    ,(UUT.wrapInside,"wrapInside")
    ] $

    \(uut,str_uut) -> describe str_uut $ do

      it "(identity)" $ do
        e <- forAllEffable genStr genWrap
        uut id e === e

      it "(mempty)" $ do
        f <- forAllWraps
        uut f mempty === mempty

      it "(distributive)" $ do
        e0 <- forAllEffable genStr genWrap
        e1 <- forAllEffable genStr genWrap
        f  <- forAllWraps
        uut f (e0 <> e1) === (uut f e0 <> uut f e1)

      it "(commutative)" $ do
        e      <- forAllEffable genStr genWrap
        f      <- forAllWraps
        g_char <- forAll $ Gen.element ['.',',']
        let g str = [g_char] ++ str ++ [g_char]
        (g <$> uut f e) === (uut f (g <$> e))


spec_wraps_functions :: Spec
spec_wraps_functions = do

  it "wrap (covariant comp.)" $ do
    e     <- forAllEffable genStr genWrap
    (f,g) <- (,) <$> forAllWraps
                 <*> forAllWraps
    UUT.wrap (f . g) e === (UUT.wrap f . UUT.wrap g $ e)

  it "wrap: asserting contra fails (meta-test)" $
    assertFailing $
    do
      (e,f,g) <- (,,)
        <$> forAllEffable genStr genWrap
        <*> forAllWraps
        <*> forAllWraps
      UUT.wrap (f . g) e === (UUT.wrap g . UUT.wrap f $ e)

  it "wrapInside (contravariant comp.)" $ do
    e     <- forAllEffable genStr genWrap
    (f,g) <- (,) <$> forAllWraps
                 <*> forAllWraps
    UUT.wrapInside (f . g) e === (UUT.wrapInside g . UUT.wrapInside f $ e)

  it "wrapEach (const f) == wrap f" $ do
    e <- forAllEffable genStr genWrap
    f <- forAllWraps
    UUT.wrapEach (const f) e === UUT.wrap f e

  describe "run" $ do
    let
      emit :: String -> ([String], ())
      emit x = (["[["] ++ [x] ++ ["]]"],())

    it "wrap" $ do
      e <- forAllEffable genStr genWrap
      f <- forAllWraps
      (===)
        ( toList (      UUT.runWith emit (UUT.wrap f e)) )
        ( toList (f <$> UUT.runWith emit e             ) )

    it "wrapInside" $ do
      e <- forAllEffable genStr genWrap
      f <- forAllWraps
      UUT.run emit (UUT.wrapInside f e) === UUT.run (f . emit) e


spec_when :: Spec
spec_when = do
  let
    emit :: String -> ([String], ())
    emit x = (["[["] ++ [x] ++ ["]]"],())
    runEmit = UUT.run emit

    it_eff desc pr = it desc $ do
      x <- forAllEffable genStr genWrap
      pr x

  it_eff "whenA: keeps structure" $ \e ->
    UUT.whenA False e === UUT.wrap (const (pure ())) e

  it_eff "whenA"       $ \e -> runEmit (UUT.whenA False        e) === pure ()
  it_eff "when' (I.) " $ \e -> runEmit (UUT.when' (pure False) e) === pure ()
  it_eff "when' (II.)" $ \e -> runEmit (UUT.when' (pure True ) e) === runEmit e


spec_byAction :: Spec
spec_byAction = do
  it "with Bool as the Enumerable" $ do
    (f,y_T,y_F) <- forAll_Bool2Eff   -- a random function
    x <- forAll $ Gen.bool           -- a random point to evaluate
    let xM :: ([String], Bool)
        xM = pure x                  -- ...lifted to the `m` of the Effable
        y = UUT.byAction xM f        -- evaluate using byAction

    -- assert that what byAction produced and values from the back-channel agree (eliminate both with `runEmit` to make them observable):
    case x of
      True ->  runEmit y===runEmit y_T
      _    ->  runEmit y===runEmit y_F

  where
    runEmit = UUT.run emit_tup


spec_byActionMaybe :: Spec
spec_byActionMaybe = do
  it "law" $ do
    (f,_,_) <- forAll_Bool2Eff
    x <- forAll $ Gen.element [True,False]
    let
      xM  = (["?"], x)
      lhs = UUT.byAction xM  f
      rhs = UUT.byActionMaybe xM (Just . f)
    runEmit lhs === runEmit rhs

  where
    runEmit = UUT.run emit_tup


spec_run :: Spec
spec_run = do
  let
    emit :: String -> ([String], ())
    emit x = (["[["] ++ [x] ++ ["]]"],())

    f_gen = Gen.element
      [ (id                ,"f:id"     )
      , (const []          ,"f:0"      )
      , (reverse           ,"f:reverse")
      , (\x -> "/"++x++"\\","f:bracket")
      ]

  describe "run" $ do
    asUnitTests $
      it "monoid homomorphism (I.)" $ UUT.run emit mempty === pure ()

    it "monoid homomorphism (II.)" $ do
      x <- forAllEffable genStr genWrap
      y <- forAllEffable genStr genWrap
      UUT.run emit (x<>y) === (UUT.run emit x *> UUT.run emit y)

    it "Naturality" $ do
      x <- forAllEffable genStr genWrap
      (f,_) <- forAllWith snd f_gen
      UUT.run emit (f<$>x) === (UUT.run (emit . f) x)

  describe "law with run" $ do
    it "runWith" $ do
      x <- forAllEffable genStr genWrap
      sequenceA_ (UUT.runWith emit x) === UUT.run emit x

    it "naturality" $ do
      x <- forAllEffable genStr genWrap
      (f,_) <- forAllWith snd f_gen
      UUT.runWith emit (f <$> x) === UUT.runWith (emit . f) x


spec_instances_laws :: Spec
spec_instances_laws = do
  Classes.functor f g genEff  -- Functor is derived, just exercise it
  Classes.applicative f genChar genEff gen_gs gen_fs
  Classes.monad genChar genEffChar genEff forAll_Str2Eff forAll_Char2Eff
  Classes.monadApplicative genEffChar gen_gs
  where
    f :: Show a => a -> String
    f = show
    g = length

    genEff  = forAllEffable genStr genWrap
    genChar = forAll Gen.alpha

    str2char = \case
      (x:_) -> x
      _     -> '-'
    genEffChar :: PropertyT IO (Effable ((,) [String]) Char)
    genEffChar = fmap str2char <$> genEff

    gen_gs :: PropertyT IO (Effable ((,) [String]) (Char -> String))
    gen_gs =
      let shw = (const "Char->String")
          genCh2Str :: Gen (Char -> String)
          genCh2Str = (\s c -> c:'_':s) <$> genStr
      in  forAllEffableWith shw genCh2Str genWrap

    gen_fs :: PropertyT IO (Effable ((,) [String]) (String -> Char))
    gen_fs =
      let shw = (const "String->Char")
      in  forAllEffableWith shw genStr2Char genWrap


spec_instances_wrapperComp :: Spec
spec_instances_wrapperComp = do
  describe "wrapper composition order" $ do

    let f = reverse

    it "for (<*>): fs on the outside" $ do
      (wf,wx) <- (,) <$> forAllWraps <*> forAllWraps
      x       <- forAll genStr
      (===)
        (UUT.singleton wf f <*> UUT.singleton wx x)
        (UUT.singleton (wf . wx) (f x))

    it "for (>>=): xs on the outside" $ do
      (wx,wf) <- (,) <$> forAllWraps <*> forAllWraps
      x       <- forAll genStr
      (===)
        (UUT.singleton wx x >>= UUT.singleton wf . f)
        (UUT.singleton (wx . wf) (f x))


spec_generator_eff :: Spec
spec_generator_eff = do

  it "values" $ do
    e <- forAllTup
    labelS (intercalate "," $ f_run e)

  it "pre-elimination #repr." $ do
    e <- forAllTup
    labelShow (innerElems e)

  it "outer lengths" $ do
    e <- forAllTup
    histogram (length $ f_run e)

  it "total chars" $ do
    e <- forAllTup
    let nChar  = sum $ length <$> (f_run e)
    histogram nChar

  where
    f_run      = fst . UUT.run emit_tup
    forAllTup  = forAllEffable genStr genWrap
    innerElems = length . UUT.unsafeDeconstruct

    histogram n = do
      classify " 0     " (n == 0)
      classify " 1- 4  " (n>=  1 && n < 5)
      classify " 5- 9  " (n>=  5 && n <10)
      classify "10-19  " (n>= 10 && n <20)
      classify "20-    " (n>= 20)


spec_generator_wrap :: Spec
spec_generator_wrap = do
    it "suppressing wraps" $ do
      f <- forAllWraps
      g <- forAllWraps
      let
        x :: ([String],())
        x = (["_"],())
        (ys,_) = f x
        (zs,_) = g (f x)
      classify "survived one (1) wrap"  $ (not . null $ ys)
      classify "survived two (2) wraps" $ (not . null $ zs)


--- Effable generators and forAll's

forAllEffable
  :: (Monad m, Show b, Show tag)
  => Gen b               -- ^ a generator of items
  -> Gen (Wrap em, tag)  -- ^ a generator of tagged wrapper functions
  -> PropertyT m (Effable em b)
forAllEffable = forAllEffableWith show


forAllEffableWith
  :: (Monad m, Show tag)
  => (b -> String)       -- ^ a way to show a generated item
  -> Gen b               -- ^ a generator of items
  -> Gen (Wrap em, tag)  -- ^ a generator of tagged wrapper functions
  -> PropertyT m (Effable em b)
forAllEffableWith showItm genS genWrps = do
  let
    gen_xs = genList $ (,) <$> genS <*> genWrps
  xs <- forAllWith show' gen_xs
  pure (mkEffs xs)
  where
    show'  xs = show    [(showItm itm,wTag)  | (itm,(_,wTag)) <- xs]
    mkEffs xs = mconcat [UUT.singleton w itm | (itm,(w,_   )) <- xs]

    genList g =
      Gen.frequency
        [ (1,Gen.constant [])
        , (9,Gen.list (Range.constant 1 4) g)
        ]


--- EffableTup ---

type EffableTup a b = Effable ((,) [a]) b

emit_tup :: a -> ([a], ())
emit_tup x = ([x],())


--- generators with EffableTup

forAll_Char2Eff :: IsString b => PropertyT IO (Char   -> EffableTup b String)
forAll_Str2Eff  :: IsString b => PropertyT IO (String -> EffableTup b String)

forAll_Char2Eff = forAll_EffableF fromEnum (\c -> [c,'@'])
forAll_Str2Eff  = forAll_EffableF length   (++ "%")


{- | Generate a random function `:: a -> EffableTup _ _`.

A random function returns an Effable that depends on the function argument.
-}
forAll_EffableF
  :: IsString b
  => (a -> Int)
  -- ^ a mapping from @a@ to any `Int`; should preferrably vary with @a@ to a reasonable extent in order to give returned functions stronger injective properties
  -> (a -> String)
  -- ^ used to create a tag that will prefix all items of the Effable the function returns
  -> PropertyT IO (a -> EffableTup b String)
forAll_EffableF mkKey mkTag = do
  table <- genTable
  let
    function x       = table !! (mkKey x `mod` nTable)
    taggedFunction x = (mkTag x ++) <$> function x
  pure taggedFunction
  where
    genTable = replicateM nTable (forAllEffable genStr genWrap)
    nTable   = 3


{- | Generate random functions `:: Bool -> EffableTup _ _`.
Also returned: a back-channel; giving the value the function returns if evaluated at True and False, respectively.
-}
forAll_Bool2Eff
  :: PropertyT IO
     ( Bool -> EffableTup String String  -- a function `f`
     , EffableTup String String          -- back-channel: result of `f True`
     , EffableTup String String          -- back-channel: result of `f False`
     )
forAll_Bool2Eff = do
  y_T <- forAllEffable genStr genWrap
  y_F <- forAllEffable genStr genWrap
  let
    f = \case True  -> y_T
              False -> y_F
  pure (f,y_T,y_F)


--- general generators

charMaterial :: [Char]
charMaterial = ['a','b','c']

genStr :: MonadGen m => m String
genStr = Gen.string (Range.constant 0 3) (Gen.element charMaterial)

genStr2Char :: Gen (String -> Char)
genStr2Char = enumsHash <$> Gen.int (Range.constant 0 25)
  where
    enumsHash :: (Enum b, Enum a) => Int -> [a] -> b
    enumsHash salt xs =
      let intHash = salt + sum (fromEnum <$> xs)
      in  toEnum $ fromEnum 'A' + (intHash `mod` 26)


--- Wrap generators and forAll's

newtype WrapTag = WrapTag { unWrapTag :: String }
  deriving (IsString)

instance Show WrapTag where
  show (WrapTag s) = "t:" ++ s
  -- unlawful; prefering concise string

genWrap :: (MonadGen m, IsString a) => m (Wrap ((,) [a]), WrapTag)
genWrap = do
  functions <- Gen.list (Range.constant 0 2) genPrim
  let
    (fs,ts) = unzip functions
    wrap (x,()) = (foldr (.) id fs $ x,())
    tag = mkTag ts
  pure (wrap,tag)

  where
    genPrim =
      Gen.elementFrequency
        [ (1, (const []         ,"x0"))
        , (2, (\xs -> xs ++ xs  ,"x2"))
        , (1, (f_bracket "<" ">","<>"))
        , (1, (f_bracket "(" ")","()"))
        , (1, (f_bracket "[" "]","[]"))
        ]

    f_bracket l r xs = [l] ++ xs ++ [r]

    mkTag [] = "id"
    mkTag ts = WrapTag . intercalate "." $ unWrapTag <$> ts


forAllWraps
  :: HasCallStack
  => (Monad m, IsString a)
  => PropertyT m (Wrap ((,) [a]))
forAllWraps =
  withFrozenCallStack $
  fst <$> forAllWith (unWrapTag . snd) genWrap


--- general helpers ---

type Prop = PropertyT IO ()

labelShow :: (MonadTest m, Show p) => p -> m ()
labelShow = label . fromString . show

{- | Modify a hspec @Spec@ so that property tests in it are run only once.
-}
asUnitTests :: Spec -> Spec
asUnitTests =
    modifyMaxShrinks (const 0)
  . modifyMaxSuccess (const 1)

it :: HasCallStack => String -> Prop -> Spec
it desc = Hspec.it desc . hedgehog

_xit :: HasCallStack => String -> Prop -> Spec
_xit desc = Hspec.xit desc . hedgehog
