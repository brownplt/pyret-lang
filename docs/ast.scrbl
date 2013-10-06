#lang scribble/manual

@(require "common.rkt")

@title[#:tag "s:ast"]{AST}

@margin-note{These bindings are not available in Captain Teach.}

The @in-code{ast} module holds a definition of the Pyret AST as a Pyret
@in-code{data} declaration, and several functions for parsing strings into
Pyret ASTs and printing them out again.

To use the @in-code{ast} module in your program, import it with:

@justcode{
import ast as <name>
}

@section{AST Functions}

@(define ast-code "
fun parse(program :: String, name :: String, options :: { check : Bool })
  -> { pre-desugar: Program, post-desugar: Program }:
  doc: 'Parses the program and returns its pre- and post- desugared AST.
  The provided name is used as the filename in source locations.
  If check is true, desugars any check and where blocks in the program.'

end

fun parse-tc(program :: String, name :: String, options :: { check : Bool, env : Any })
  -> Program:
  doc: 'Parses and \"type-checks\" the program, checking for unbound identifiers
  and wrapping any annotation-based contracts around expressions.
  The provided name and check are as in parse.
  The fields of env are treated as bound variables.  In the future this will be a richer
  datastructure that allows the specification of a more detailed type environment.'
end

fun to-native(program :: Program):
  doc: 'Creates a native representation of the AST suitable for passing to eval()'
end
")
@(define ast-functions-ast (parse-pyret ast-code))

@(pretty-functions ast-functions-ast '(
  parse
  parse-tc
  to-native
  ))

@section{AST Datatypes}

@(define ast-ast (get-pyret-lib "lang/racket-ffi/ast.arr"))

@(label-data (get-decl ast-ast 'Program))
@(pretty-data (get-decl ast-ast 'Program))

@(label-data (get-decl ast-ast 'Header))
@(pretty-data (get-decl ast-ast 'Header))

@(label-data (get-decl ast-ast 'ImportType))
@(pretty-data (get-decl ast-ast 'ImportType))

@(label-data (get-decl ast-ast 'Expr))
@(pretty-data (get-decl ast-ast 'Expr))

@(label-data (get-decl ast-ast 'Bind))
@(pretty-data (get-decl ast-ast 'Bind))

@(label-data (get-decl ast-ast 'Member))
@(pretty-data (get-decl ast-ast 'Member))

@(label-data (get-decl ast-ast 'ForBind))
@(pretty-data (get-decl ast-ast 'ForBind))

@(label-data (get-decl ast-ast 'IfBranch))
@(pretty-data (get-decl ast-ast 'IfBranch))

@(label-data (get-decl ast-ast 'CasesBranch))
@(pretty-data (get-decl ast-ast 'CasesBranch))

@(label-data (get-decl ast-ast 'Ann))
@(pretty-data (get-decl ast-ast 'Ann))

@(label-data (get-decl ast-ast 'AField))
@(pretty-data (get-decl ast-ast 'AField))

