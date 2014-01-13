#lang scribble/manual

@(require
  racket/list
  "common.rkt")

@(define cmdline-ast (get-pyret-lib "lang/racket-ffi/cmdline.rkt"))
@(define parsed-arguments (get-decl cmdline-ast 'ParsedArguments))
@(define param-repeats (get-decl cmdline-ast 'ParamRepeat))
@(define params (get-decl cmdline-ast 'Param))

@title[#:tag "s:cmdline"]{Handling Command-line Arguments}

Pyret has support for processing command-line arguments.  Naturally, it is only available
offline; to use it, include

@(justcode "import cmdline as C")

at the top of your source file.  Here is a complete example usage:

@justcode{
#lang pyret

import cmdline as C
import format as F

_ = print(F.format("\nInvoked on filename ~a, with params ~a\n", [C.file-name, torepr(C.args)]))

# Define a parameter-parsing dictionary...
options = {
  foo: C.flag(C.once, "Does foo"),
  bar: C.equals-val(C.Number, C.many, "Several bars"),
  baz: C.next-val-default(C.Bool, true, some("b"), C.required-once, "Bazes")
}

# Use it to print out calling-convention information
_ = print(C.usage-info(options).join-str("\n") + "\n")
# Or use it to parse supplied options
_ = print(C.parse-cmdline(options))
}

This produces the following output:

@justcode{
> raco pyret test.arr --bar=5 --baz false --bar=6 other "*" --bar=7 -foo

Invoked on filename tests.arr, with params ["--bar=5", "--baz", "false", "--bar=6", "other", "*", "--bar=7", "-foo"]

Usage: tests.arr [options] where:
  --bar=<number>: Several bars (may be repeated)
  -foo: Does foo (may be used at most once)
  --baz [(true|false)]: Bazes (must be used exactly once, default: true)
  -b: Defaults for Bazes (must be used exactly once)

success({bar: [5, 6, 7], foo: true, baz: false}, ["other", "*"])
}
@section[#:tag "s:main-functions"]{Main functions}
@subsection{Parsing functions}
The Command-line module supports two main parsing functions:

@(pretty-functions cmdline-ast '(parse-args parse-cmdline))

These both consume a dictionary of @seclink["s:params" "parameters"], and produce:

@(label-data parsed-arguments)
@(pretty-data parsed-arguments)

If parameter parsing succeeds, the returned object contains a dictionary
@tt{parsed} of the parameter values corresponding to the option definitions
passed in, and a @tt{unknown} list of the remaining parameters.  If parameter
parsing fails, the @tt{arg-error} result contains the error message as well as
any parameters that had been successfully parsed.

Parsing may fail because two option definitions were specified with the same
name, because an actual command-line parameter could not be parsed to the
intended type, because a parameter value was missing, because the command-line
parameters were missing required options, because the command-line
parameters included some options too many times, or because a command-line
parameter looked like some option but was of the wrong form.

@subsection{Utility functions} 
Additionally, the Command-line module exposes a function to print usage
information: 

@(pretty-functions cmdline-ast '(usage-info))

@section[#:tag "s:params"]{Parameter definitions}
Pyret supports five types of parameters:

@(label-data params)
@(pretty-data params)

These respectively support parameters like @tt-nodecode{-name},
@tt-nodecode{--name=val} (possibly with a default value, written just
@tt-nodecode{--name}, or via a shorter alias @tt-nodecode{-n}), and
@tt-nodecode{--name val} (also possibly with a default value, written
@tt-nodecode{--name}, or via a shorter alias @tt-nodecode{-n}) Each parameter
specifies a @tt{desc}ription to be used in the usage text, and a @tt{repeated}
flag to specify how many times the parameter may (or must) be used.
Additionally, all parameter types except simple @tt{flag}s specify a parser to
convert an actual command-line parameter string into a typed value, and may
also specify the default value for the parameter.  Note that the concrete
syntax of ``flag-looking'' paramters @tt-nodecode{-name} may be either a
@tt{flag}, a @tt{equals-val-default} or a @tt{next-val-default}; care should be
exercised to make sure the command-line arguments make sense!


@(define parse-params (get-decl (parse-pyret
  "data ParseParams:
    | Number
    | Bool
    | String
    | Custom(name :: String, parser :: (Number, String, String -> Any))
   end") 'ParseParams))
@(label-data parse-params)
@(pretty-data parse-params)

The first three constructors try to produce a numeric, boolean, or string value
from a provided string parameter.  The final @tt{Custom} constructor allows
parsing string parameters into values of a custom data type, and is given the
position in the parameter list, the intended key name, and the potential
parameter value.  For example,

@justcode{
  data RGB: red | green | blue end
  custom-parser = Custom("red|green|blue", fun(arg-index, name, val):
      if val == "red": red
      else if val == "green": green
      else if val == "blue": blue
      else: raise(format("Parameter ~a for key-name ~a expected an RGB argument, got ~a", 
              [arg-index, name, torepr(val)]))
      end
    end)
}

Note: the exported names for these @tt{ParseParams} constructors are not the
internal names, and so cannot be used in a @tt{cases} expression --- but in
general, such @tt{cases} shouldn't be necessary.

@(label-data param-repeats)
@(pretty-data param-repeats)
