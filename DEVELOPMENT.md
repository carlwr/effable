# Development

### Enable project settings suitable for local development

-> create this file:
```haskell
--- ./cabal.project.local ---
import: misc/dev.project
```

### Test-build with `cabal.project.local` ignored

If a `cabal.project.local` is present it can be ignored with:
```sh
cabal --project-file=cabal.project.no-local ..
```

Note: `cabal.project.no-local` imports `cabal.project` so that file stays active.

_Warning: Cabal freshness checks won't work._ If `cabal.project` is edited, then, before `cabal` is invoked again, do one of:

```sh
touch cabal.project.no-local
rm -rf dist-newstyle/cache/plan.json
```

### Testing

```sh
cabal build all && cabal test all

# to give hspec test options:
cabal test effable-hspec:hspec --test-options='--no-color'
cabal test effable:doctest
```

Build and test with `stack`:
```sh
stack test test/hspec
```

**Notes:**
- Note: doctests may be flaky on first run after an edit; `cabal build && ..` typically mitigates that. On odd errors from `doctest-parallel`; first try to run immediately again.
- `hspec-tidy-formatter` depends on `effable`; hence, if mutating the lib during testing to inject bugs, use the default hspec formatter instead

### Trigger CI manually

```sh
gh workflow run ci.yml --ref <branch>
```

### Other useful commands

```sh
cabal haddock --haddock-for-hackage

./scripts/make-readme > ./README.md

./scripts/test-tested-with
```
