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
