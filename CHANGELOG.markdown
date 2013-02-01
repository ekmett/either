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
