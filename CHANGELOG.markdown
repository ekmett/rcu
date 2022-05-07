## next [????.??.??]
* Make the `IncCounterExperiment` benchmark compile with GHC 9.4.

## 0.2.5 [2021.02.17]
---------------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

## 0.2.4 [2019.05.02]
---------------------
* Support building with `base-4.13` (GHC 8.8).

## 0.2.3 [2018.08.01]
---------------------
* Add `MonadFail` instances for `ReadingRCU` and `WritingRCU`.

## 0.2.2 [2018.02.06]
---------------------
* Include `HLint.hs` with the tarball distribution, fixing the `hlint`
  test suite.

## 0.2.1
--------
* Support `doctest-0.12`

## 0.2
------
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

## 0
----
* Initial version

