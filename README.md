# refs.el
[![Build Status](https://travis-ci.org/Wilfred/refs.el.svg?branch=master)](https://travis-ci.org/Wilfred/refs.el)

An Emacs package for finding references to elisp functions, macros or
variables.

This is particularly useful for finding all the places a function is
used, or finding examples of usage.

## What about Etags?

Etags is a program shipped with Emacs that can be used for indexing
code to find definitions. To my knowledge, it cannot find references.

[universal-ctags](https://github.com/universal-ctags/ctags) (formerly
known as exuberant-ctags) does provide the ability to find references,
however.

Unlike these projects, refs.el considers code loaded in the current
Emacs instance and requires no manual configuration. It's also smarter
about detecting function calls (e.g. we can spot `funcall`
references).

## Similar Projects

[el-search](https://elpa.gnu.org/packages/el-search.html)
