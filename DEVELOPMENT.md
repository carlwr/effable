# Development

tested with Cabal version: `3.14.2.0`

### Enable project settings suitable for local development

-> create this file:

```haskell
--- ./cabal.project.local ---
import: misc/dev.project
```

### Useful commands

```sh
cabal test
cabal test -fdoctest  # if `dev.project` is not active

cabal haddock --haddock-for-hackage

./scripts/make-readme > ./README.md

./scripts/test-tested-with

stack test --flag effable:doctest
```
