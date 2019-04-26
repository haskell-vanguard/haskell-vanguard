Operation Vanguard
====

This is a project that tries to maintain a set of Cabal packages that are

* Buildable on GHC 8.8
* All compatible
* available as Git repositories

Building
----

Warning: submodules may not be specified correctly.

```
git clone --recursive git@github.com:haskell-vanguard/haskell-vanguard
cd haskell-vanguard
cabal new-build
```
