## rcu

[![Build Status](https://secure.travis-ci.org/ekmett/rcu.png?branch=master)](http://travis-ci.org/ekmett/rcu)

This package is an exploration of Read-Copy Update in Haskell based on [Relativistic Programming in Haskell](http://web.cecs.pdx.edu/~walpole/papers/haskell2015.pdf) by Cooper and Walpole.  It includes STM- and QSBR-based implementations.

In the spirit of [A Relativistic Enhancement to Software Transactional Memory](https://www.usenix.org/legacy/events/hotpar11/tech/final_files/Howard.pdf)
 by Howard and Walpole, we could extend the STM implementation to allow reads and
writes on the same data in parallel, writes to disjoint data in parallel, and
force readers to agree that writes before a `synchronize` happened before
writes after it.

## Contact Information

Contributions and bug reports are welcome!

Please feel free to contact us through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
-Ted Cooper
