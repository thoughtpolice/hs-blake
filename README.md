# Minimal package for blake hashes

[![Build Status](https://travis-ci.org/thoughtpolice/hs-blake.png?branch=master)](https://travis-ci.org/thoughtpolice/hs-blake)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](http://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://www.haskell.org)

This package implements minimal bindings to the [blake][] hash
function, which was an SHA-3 finalist. BLAKE is small, simple to
implement and built on analyzed components. It should be relatively
easy to both depend on, or include outright in your executable/package
itself.

The underlying implementation is the `ref` code of `blake256` and
`blake512` from [SUPERCOP][], which was originally implemented by
Jean-Philippe Aumasson.

[blake]: https://131002.net/blake/
[SUPERCOP]: http://bench.cr.yp.to/supercop.html

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install blake
```

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-blake.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-blake.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-blake/master/AUTHORS.txt).

# License

MIT. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/hs-blake/master/LICENSE.txt)
for terms of copyright and redistribution.

[contribute]: https://github.com/thoughtpolice/hs-blake/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/hs-blake/issues
[gh]: http://github.com/thoughtpolice/hs-blake
[bb]: http://bitbucket.org/thoughtpolice/hs-blake
[Hackage]: http://hackage.haskell.org/package/blake
