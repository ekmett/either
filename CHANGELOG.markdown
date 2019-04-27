Next [????.??.??]
-----------------
* Only incur a `semigroups` dependency on pre-8.0 GHCs.

5.0.1 [2018.07.03]
------------------
* Make the `Semigroup`, `Apply`, and `Applicative` instances for `Validation`
  lazier.
* Make `vap` lazier in its second argument.
* Introduce `vapm`, an even lazier version of `vap` which requires a
  `Monoid` constraint. Also add `apm`, a counterpart for `Validation`.
* Use `test-framework` and `QuickCheck` in the test suite.

5
-
* Changed the semantics of the `Validation` `Alt` and `Alternative` instances to collect errors.
  The previous implementation did not correctly abide the laws.
* Added `vap`, for when users want validation like semantics but don't want to convert back and forth to validation all the time. Similarly, added `ealt` to give either's `Alt` semantics to validation.
* Dropped the deprecated `Control.Monad.Trans.Either`.  Use `Control.Monad.Trans.Except` from `transformers` and/or
  `transformers-compat` instead.

4.5
----
* Add `MMonad` instance for `EitherT`
* Deprecate `Control.Monad.Trans.Either` in favor of `Control.Monad.Trans.Except`
* Add `firstEitherT`

4.4.1.1
-------
* Fixed building on newer GHCs. (type synonyms require explicit foralls for unused variables these days)

4.4.1
-----
* `transformers` 0.5 support
* Documentation fixes

4.4
---
* Support `mmorph`

4.3.4.1
-------
* Support `MonadRandom` 0.4

4.3.4
-----
* Support `bifunctors` 5, `profunctors` 5, and `semigroupoids` 5.

4.3.3.3
-------
* Fixed and enhanced documentation for `eitherToError`.

4.3.3.2
-------
* Support `exceptions` 0.8

4.3.3.1
-------
* Support `exceptions` 0.7

4.3.3
-----
* Added `eitherToError`.

4.3.2.1
-------
* Support `monad-control` 1.0

4.3.2
-----
* Added `Validation`.

4.3.0.2
-------
* Updated MonadRandom support.

4.3.0.1
-------
* Fixed import of `MonadCatch` to support versions of `base` before 4.6

4.3
---
* Inverted dependency between `free` and `either`.

4.2
---
* Added instances for `MonadThrow`, `MonadCatch`.

4.1
---
* Added instances for `MonadBase`, `MonadBaseControl`, and `MonadTransControl`.

4.0
---
* Updated dependencies.

3.4.2
-----
* Added 'Data.Either.Combinators'.

3.4.1
-----
* Trustworthy despite UndecidableInstances

3.4
---
* Delegate `fail` to the underlying `Monad`, rather than `error`.

3.3
---
* Inverted roles between `Semigroup` and `Alt`. This let us write `Alternative` and `MonadPlus` instances that are compatible.
* Removed the `Functor` constraint on most instances in exchange for incurring a `Monad` constraint on `Traversable`. `EitherT`
  is after all, a `Monad` transformer first and foremost.

3.2
---
* Changed the `Semigroup` to use a `Semigroup` to combine `Left` branches. Left `Alt` untouched, so you can mix and match.

3.1
---
* Added instances for `mtl` classes and `MonadRandom`.
* The meaning of `mapEitherT` has changed to match `mapErrorT` in the `mtl`. The old `mapEitherT` is now `bimapEitherT`.

3.0.3
-----
* Started `CHANGELOG`
