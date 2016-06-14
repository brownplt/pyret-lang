#lang scribble/base

@(require
  "../../scribble-api.rkt"
  (only-in racket/list add-between)
  scribble/core)

@title[#:tag "runtime"]{Runtime API}

Pyret exposes most of its internal operations on the @tt{runtime} object; most
JavaScript code that interacts with Pyret will need to know about the runtime.

The library that defines runtimes is in @tt{src/js/base/runtime-anf.js}, and it
is configured to be importable with @tt{js/runtime-anf} via RequireJS:

@verbatim{
define(["js/runtime-anf"], function(runtimeLib) {
  // use runtimeLib
});
}

@doc-internal["RuntimeLib" "create" (list "options") "Runtime"]

Create a new Pyret runtime.  The @tt{RuntimeLib} value itself only exports
this interface, and most other useful functions are referenced from the
runtime objecs it creates.  The options are:

@verbatim{
  options :: {
    initialGas: Number,
    stdout: String → Undefined,
    stderr: String → Undefined
  }
}

The size of the Pyret stack is constrained to @tt{initialGas} frames; most
applications have little need to set this.

For applications that need control over printing, they can set @tt{stdout} and
@tt{stderr} to get called whenever Pyret would print strings (e.g. via
@tt{print}).  This interface may change to accept all Pyret values at some
point, to allow for richer rendering interfaces.

@section[#:tag "s:runtime-object"]{The Pyret Runtime}

The return value of @internal-id["RuntimeLib" "create"] is a runtime
object with many useful methods for programmatically interacting with Pyret.

@subsection{Running Pyret Programs}

@doc-internal["Runtime" "runStandalone" (list "modules: JSDict<URI, StaticModules>" "dependencies: JSDict<URI, JSDict<String, URI>>" "toLoad: JSArray<URI>" "postLoadHooks: JSDict<URI, (PyretModuleResult → Undefined)>") "PyretModuleResult" #:stack-unsafe #t]

Uses @tt{toLoad}—the list of URIs—to evaluate modules in order.  The modules
are found in the @tt{modules} dictionary, and the @tt{dependencies} dictionary
describes which modules they depend on.  This structure is described in
@secref["s:complete"].

A more important feature of @internal-id["Runtime" "runStandalone"] is the
ability to register @tt{postLoadHooks}.  This dictionary, keyed on URI,
contains callbacks which are invoked on completion of the corresponding module,
and passed the module's result.  The most obvious candidate for a
@tt{postLoadHook} is the main module, or the last one in the @tt{toLoad} list;
this is where test results can be fetched and printed and errors reported,
based on the returned [REF PyretModuleResult].

In addition, Pyret's default standalones register several @tt{postLoadHook}s.
For example, after the @tt{ffi} library is loaded, a number of new fields are
added to @tt{runtime} to manipulate lists.  Other uses include:

@itemlist[

@item{Substituting a different checker library by setting @tt{current-checker}
after loading the appropriate library.}

@item{Logging the completion time for loading each module in the @tt{toLoad}
list for benchmarking.}

]

While evaluating the modules, the runtime caches the results for each module
that completed successfully, in the @tt{runtime.modules} dictionary.  This can
be accessed later to quickly get the exported values of a module without
re-running it.

@subsection{Creating Values}

@doc-internal["Runtime" "makeNumber" (list "JSNumber") "PyretNumber"]

@doc-internal["Runtime" "makeNumberFromString" (list "JSString") "PyretNumber"]

Parses the string and creates a representation of the number that avoids float
overflows and can represent very large and very small rationals exactly.

@doc-internal["Runtime" "makeString" (list "JSString") "PyretString"]

The representation of Pyret strings is JS strings, though this may change to
accommodate better Unicode support in the future.

@doc-internal["Runtime" "pyretTrue" #f "PyretBoolean"]
@doc-internal["Runtime" "pyretFalse" #f "PyretBoolean"]

The runtime values for @pyret{true} and @pyret{false} in Pyret.  Representation
is JavaScript @tt{true} and @tt{false}.

@doc-internal["Runtime" "makeArray" (list "JSArray") "PyretRawArray"]

Creates a Pyret @pyret-id["RawArray" "raw-arrays"] with the given elements.
Currently the identity function: Pyret raw arrays are JavaScript arrays.

@doc-internal["Runtime" "makeObject" (list "JSObj") "PyretObject"]

Creates a Pyret object with the fields in the input object.  The representation
of an object is @emph{not} one-to-one with JS objects.

The fields of a Pyret object go in the @tt{dict} field of the JS object, and
the JS object has an additional field called @tt{brands}, which hold
information about an object's type information (if it has any).
@pyret-id["StringDict" "string-dict"]s, for example, are branded objects.

@doc-internal["Runtime" "makeFunction" (list "JSFunction") "PyretFunction"]

Returns a Pyret function backed by the provided JS function.  The Pyret
function will @emph{not} do any arity checks on behalf of the JS function, so
any arity checks need to be done explicitly (see @internal-id["Runtime"
"checkArity"]).  The function can be accessed directly via the @pyret{app}
field of the Pyret function, but read the section on @internal-id["Runtime"
"safeCall"] in order to suitably protect calls to Pyret functions.

@doc-internal["Runtime" "makeMethodN" (list "JSFunction") "PyretMethod"]


@subsection{Interacting with Objects}

@doc-internal["Runtime" "getField" (list "PyretObject" "JSString") "PyretValue"]

Gets the field with the given name from the object.  If the field is a method,
it is automatically curried over the object, as with dot.

@doc-internal["Runtime" "getColonField" (list "PyretObject" "JSString") "PyretValue"]

Gets the field with the given name from the object.  If the field is a method,
no additional work is performed.

@doc-internal["Runtime" "getFields" (list "PyretObject") "JSArray<String>"]

Returns all the field names of the given object.

@doc-internal["Runtime" "hasField" (list "PyretObject" "JSString") "JSBoolean"]

Checks if the given object has the named field.

@subsection{Assertions}

@doc-internal["Runtime" "checkArity" (list "JSNumber" "Arguments" "JSString") "Undefined"]

Checks that the given argument list has the given arity.  Throws an exception
if they don't match.

There are a number of checking functions that check that a given argument is of
a particular type, and throw an exception if not:

@doc-internal["Runtime" "checkNumber" (list "Any") "Undefined"]
@doc-internal["Runtime" "checkString" (list "Any") "Undefined"]
@doc-internal["Runtime" "checkBoolean" (list "Any") "Undefined"]
@doc-internal["Runtime" "checkObject" (list "Any") "Undefined"]
@doc-internal["Runtime" "checkFunction" (list "Any") "Undefined"]
@doc-internal["Runtime" "checkMethod" (list "Any") "Undefined"]
@doc-internal["Runtime" "checkArray" (list "Any") "Undefined"]



@doc-internal["Runtime" "checkPyretVal" (list "Any") "Undefined"]

@subsection{Equality}

@doc-internal["Runtime" "combineEquality" (list "EqualityResult" "EqualityResult") "EqualityResult"]

Takes two @pyret-id["EqualityResult" "equality"]s and combines them.  Any value
paired with @pyret-id["NotEqual" "equality"] produces @pyret-id["NotEqual"
"equality"], any combination of @pyret-id["Equal" "equality"] and
@pyret-id["Unknown" "equality"] produces @pyret-id["Unknown" "equality"], and
two @pyret-id["Equal" "equality"] values produce @pyret-id["Equal" "equality"].

@subsection{FFI Helpers}

@doc-internal["Runtime" "ffi" #f "FFIHelpers"]

The Pyret runtime instantiates an @seclink["ffi" (list @tt{FFIHelpers} " object")] and
stores in in the @tt{ffi} field.

