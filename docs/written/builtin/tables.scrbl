#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
  '(module "tables"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Table")
      (variants)
      (shared))))

@docmodule["tables" #:noimport #t #:friendly-title "Tables"]{
  @type-spec["Table" (list)]

  The type of a table.

  A table can be constructed by the table syntax.

  @examples{
    my-table = table: col1 :: Number, col2 :: String
        row: 1, "foo"
        row: -123, "bar"
      end
  }

  The above example will construct a table with two columns: @a-field[@tt{col1} N]
  and @a-field[@tt{col2} S]. Please note that the order of columns matters.
  That is, @pyret-block{table: col1 :: Number, col2 :: String} is not the same as
  @pyret-block{table: col2 :: String, col1 :: Number}.
}
