# `Effable`

_A data structure for emission plans_

![GitHub License](https://img.shields.io/github/license/carlwr/effable)
![Hackage Version](https://img.shields.io/hackage/v/effable)

<br>


```haskell
-- λ> import Effable
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
* [API documentation](https://hackage.haskell.org/package/effable/docs/Effable.html)
* [github.com/carlwr/effable](https://github.com/carlwr/effable)

---
<br>

{hsModule}

# Written by a human

AI tools were used extensively during the development of this package - for exploration, discussions, and feedback. All code and documentation were however authored by me (Carl), a human developer: no text in this repository is direct output from an AI model.
