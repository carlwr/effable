# Development

tested with Cabal version: `3.14.2.0`

### Enable project settings suitable for local development

-> create this file:

```haskell
--- ./cabal.project.local ---
import: misc/dev.project
```

### Testing

```sh
cabal build && cabal test
cabal build && cabal test -fdoctest  # if `dev.project` is not active
```

Note: doctests may be flaky on first run after an edit; `cabal build && ..` typically mitigates that. On odd errors from `doctest-parallel`; first try to run immediately again.

### Trigger CI manually

```sh
gh workflow run ci.yml --ref <branch>
```

### Other useful commands

```sh
cabal haddock --haddock-for-hackage

./scripts/make-readme > ./README.md

./scripts/test-tested-with

stack test --flag effable:doctest
```
