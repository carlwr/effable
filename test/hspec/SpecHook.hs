module SpecHook
( hook
) where

import Test.Hspec.TidyFormatter qualified as TidyFormatter

import Test.Hspec
import Test.Hspec.Core.Spec (modifyConfig)
import Test.Hspec.Runner (Config(..), ColorMode(ColorAlways))


hook :: Spec -> Spec
hook = (setupSpec >>) . TidyFormatter.use

setupSpec :: Spec
setupSpec = modifyConfig $
    (\c -> c { configSeed           = Just seed   })
  . (\c -> c { configColorMode      = ColorAlways })
  . (\c -> c { configConcurrentJobs = Just 1      })
  . (\c -> c { configRandomize      = False       })
  where
    seed = 1
