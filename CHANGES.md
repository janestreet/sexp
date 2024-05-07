
## Release v0.17.0

- [index] expressions in [sexp query] now support negative indexes. -1 selects the last
  element in a list, -2 selects the second-to-last element, and so on.

- added [sexp pretty] as an alias for [sexp pp]

- [sexp select] now allows selecting on strings with spaces

- The [pp] subcommand can now take a file argument rather than always having to read from
  stdin.

- Similarly, [sexp validate] can now take one or more filenames as arguments

- Added a subcommand [sort] for sorting a sequence of sexps.

- [sexp to-json] time complexity when merging arrays and objects went from O(n^2) to
  O(n log n).

- Fixed a [sexp change] bug that was disabling the variable syntax escaping in [rewrite]
  expressions.

- Added code for a [sexp-group] command for grouping a sequence of sexps by some
  subexpression.  This will eventually become a subcommand of the [sexp] executable. For
  now it lives in tmp-bin/

- added a -drop flag to [sexp select] and [sexp multi-select] that will output the
  original sexp with the matching fields removed, rather than printing out the matches
  themselves.

## Release v0.16.0

- added a subcommand [sexp validate] that checks whether an input parses as a sexp
- exposed a new value [Sexp_sort.command].  We are testing this now and it will eventually
  become the [sexp sort] subcommand.
