Sexp - The s-expression toolkit
===============================

A suite of tools for working with s-expressions from the command line.
It contains subcommands for pretty printing, querying, and modifying
sexps, as well as some conversions to and from other formats.

See also:

- [Sexp query by example](doc/query_by_example.md)
- [Sexp query formal semantics](doc/query_semantics.md)
- [Sexp change formal semantics](doc/change_semantics.md)
- [Sexp change by example](doc/change_by_example.md)

```sh
$ sexp -help
the s-expression toolkit

  sexp SUBCOMMAND

=== subcommands ===

  assemble        Assemble a lists of parts into sexps. Sexp part lists are
                  separated by newlines.
  change          transform an s-expression
  flatten         Flatten a list of sexp into its parts. Each part on its own
                  line.
  get             extract parts of an s-expression
  of-xml          convert XML from stdin into an s-expression
  pp              Pretty print S expressions in a human-friendly way.
  print           pretty-print an s-expression
  query           query an s-expression
  resolve-macros  resolve macros in a sexp
  restructure     recover structure of an s-expression
  select          Use CSS-style selectors to traverse sexp trees
  to-csv          converts a list of record s-expressions into CSV format
  validate        validate a sequence of s-expressions on stdin
  version         print version information
  help            explain a given subcommand (perhaps recursively)
```

## Examples

Colorize the output of `ocamlc -dlambda`:

<!-- HTMLized output of "sexp pp -color" -->
<style type="text/css">
    <!--
      .ATTRLIST {
        /* (foreground-color . "magenta") */
        color: #aa00aa;
      }
      .ATTRLIST-1 {
        /* (foreground-color . "white") */
        color: #808080;
      }
      .ATTRLIST-2 {
        /* (foreground-color . "yellow") */
        color: #eeaa00;
      }
      .ATTRLIST-3 {
        /* (foreground-color . "cyan") */
        color: #00aaaa;
      }
    -->
</style>
<pre class="console-output">
$ ocamlc -dlambda -c fact.ml 2>&amp;1 | sexp pp -color
<span class="ATTRLIST-2">(setglobal</span> Fact! <span class="ATTRLIST-3">(</span>
  <span class="ATTRLIST-3">letrec</span>
  <span class="ATTRLIST-1">(fact/1008</span> <span class="ATTRLIST">(</span>
    <span class="ATTRLIST">function</span> n/1009 <span class="ATTRLIST-2">(</span>
      <span class="ATTRLIST-2">if</span> <span class="ATTRLIST-3">(==</span> n/1009 0<span class="ATTRLIST-3">)</span> 1 <span class="ATTRLIST-3">(*</span> n/1009 <span class="ATTRLIST-1">(apply</span> fact/1008 <span class="ATTRLIST">(-</span> n/1009 1<span class="ATTRLIST">)</span><span class="ATTRLIST-1">)</span><span class="ATTRLIST-3">)</span><span class="ATTRLIST-2">)</span><span class="ATTRLIST">)</span><span class="ATTRLIST-1">)</span>
  <span class="ATTRLIST-1">(makeblock</span> 0 fact/1008<span class="ATTRLIST-1">)</span><span class="ATTRLIST-3">)</span><span class="ATTRLIST-2">)</span>
</pre>

Extract the list of command run by jenga:

```
$ cat query.sexp
(pipe
  (variant Job_started 1)
  (index 1)
  (wrap (cat (field prog) (pipe (field args) each)))
)
$ sexp query -file query.sexp < .jenga/.jenga.debug
(ocamlc -c foo.mli)
(ocamlc -c foo.ml)
...
```
