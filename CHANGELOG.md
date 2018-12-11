## v1.4

Fixed an issue when running elisp-refs on Emacs master, which renamed
`format-proper-list-p` to `proper-list-p` (and moved it to subr.el).

Fixed an issue where the results buffer showed the wrong lines if
comments contained parentheses.

Fixed an issue where arguments in `declare-function` were confused
with a function call.

## v1.3

* Refs buffers now have names of the form `*refs: foo*`.
* Fixed an issue with a dependency on a loop.el version that doesn't
  exist.

## v1.2

* You can now filter search results to a directory. This is useful
  when working on large elisp codebases, and it's faster too.
* Results buffers now include a link to describe the thing being
  searched for.

## v1.1

* Rebranded to elisp-refs.
* Commands are now autoloaded.
* Added examples to the readme of cases that we can't detect.
* Sharp-quoted function references are now highlighted with context.
* Give more feedback on first run, when we're decompressing .el.gz
  files.
* Searches now default to the symbol at point.

## v1.0 

Initial release.

