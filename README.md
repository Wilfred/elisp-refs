# refs.el
[![Build Status](https://travis-ci.org/Wilfred/refs.el.svg?branch=master)](https://travis-ci.org/Wilfred/refs.el)

An Emacs package for finding references to elisp functions, macros or
variables.

This is particularly useful for finding all the places a function is
used, or finding examples of usage.

TODO: screenshots

## Running tests

You can run the tests with:

```
$ cask install
$ cask exec ert-runner
```

## Performance

refs.el is CPU-intensive elisp and has been carefully optimised. You
can run the benchmark script with:

```
$ cask install
$ ./bench.sh
```

New features are carefully measured to ensure performance does not get
worse.

## Alternative Projects

**TAGS**: It is possible to record function references in TAGS
files. Whilst [universal-ctags](https://github.com/universal-ctags/ctags) (formerly
known as exuberant-ctags) does provide the ability to find references,
it is not supported in its lisp parser.

etags, the TAGS implementation shipped with Emacs, cannot find
references (to my knowledge).

**el-search**:
[el-search](https://elpa.gnu.org/packages/el-search.html) allows you
to search for arbitrary forms in elisp files. It's slower, but a much
more general tool. Its design greatly influenced refs.el.
