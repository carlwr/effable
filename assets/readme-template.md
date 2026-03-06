# `Effable`

_A data structure for emission plans_

[![License](https://img.shields.io/badge/license-MIT-green)](./LICENSE)
[![Hackage Version](https://img.shields.io/hackage/v/effable)](https://hackage.haskell.org/package/effable)
![CI](https://img.shields.io/github/actions/workflow/status/carlwr/effable/ci.yml?label=CI)

<br>


```haskell
-- λ> import Data.Effable
-- λ> :info Effable

type Effable :: (* -> *) -> * -> *
newtype Effable m b

instance Semigroup   (Effable m b)
instance Monoid      (Effable m b)

instance Functor     (Effable m)
instance Applicative (Effable m)
instance Monad       (Effable m)

instance Alternative (Effable m)
instance MonadPlus   (Effable m)

instance IsString b => IsString (Effable m b)
```

Links:
* [Hackage page](https://hackage.haskell.org/package/effable)
* [API documentation](https://hackage.haskell.org/package/effable/docs/Data-Effable.html)
* [github.com/carlwr/effable](https://github.com/carlwr/effable)

---
<br>

{hsModule}

# Written by a human

During the development of this package, AI models were used extensively for discussions and feedback. All code and documentation however is authored by me (Carl), a human developer: no text (code; natural language) within this package/repo is direct output from an AI model.

Since I am not a native English speaker, any natural language is likely to feature language quirks. AI models were not asked to identify or rectify such.

The above should not be understood as any opinion or even preference of mine - I both use and value development with higher degrees of AI autonomy than what was used in this project.
