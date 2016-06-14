#lang scribble/base


@(require
  "../../scribble-api.rkt"
  (only-in racket/list add-between)
  scribble/core)

@title[#:tag "ffi"]{FFI Helpers}

There are a number of convenience functions that aren't native to the Pyret
runtime, but are often used in JavaScript code that interacts with the runtime.
Some of the most commonly used ones are documented here; this list is often
growing.

@section{Equality}

The ffi exposes several utilities related to @secref["equality"].

@doc-internal["FFI" "equal" #f "PyretObject"]

The @pyret-id["Equal" "equality"] value.

@doc-internal["FFI" "unknown" #f "PyretObject"]

The @pyret-id["Unknown" "equality"] value.

@doc-internal["FFI" "notEqual" #f "PyretFunction"]

The @pyret-id["NotEqual" "equality"] constructor.

@doc-internal["FFI" "isEqual" (list "Any") "JSBoolean"]

Checks if the given value is @pyret-id["Equal" "equality"].

@doc-internal["FFI" "isNotEqual" (list "Any") "JSBoolean"]

Checks if the given value is an instance of @pyret-id["NotEqual" "equality"].

@doc-internal["FFI" "isUnknown" (list "Any") "JSBoolean"]

Checks if the given value is a @pyret-id["Unknown" "equality"].

@doc-internal["FFI" "isEqualityResult" (list "Any") "JSBoolean"]

Checks if the given value is an instance of a @pyret-id["EqualityResult" "equality"].


@section[#:tag "ffi:exceptions"]{Exceptions}

FFI helpers provide the easiest way to programmatically throw Pyret exceptions
from JavaScript.  Most commonly, user-defined modules will simply throw
@tt{MessageExceptions} that contain a string describing the error.

@doc-internal["FFI" "throwMessageException" (list "PyretString") "Undefined"]

Throws an exception that Pyret recognizes and reports with a stack trace, using
the provided string as the message.

@doc-internal["FFI" "makeMessageException" (list "PyretString") "Error"]

Sometimes its useful to @emph{create} an exception without actually throwing
it, like when using the @tt{error} callback of the @tt{Restarter} in
@internal-id["Runtime" "pauseStack"].  This call creates a new exception object
without throwing it.

@section[#:tag "ffi:lists"]{Lists}

Pyret lists are ubiquitous in Pyret's internals and libraries, and this library
provides a few conveniences for working with them.

@doc-internal["FFI" "makeList" (list "JSArray") "List"]

Turns a JavaScript array into a Pyret @pyret-id["List" "lists"] with the same
elements in the same order.

@doc-internal["FFI" "toArray" (list "List") "JSArray"]

Turns a Pyret @pyret-id["List" "lists"] with the same elements in the same
order.  For doing computationally heavy work, sometimes it is useful to convert
a Pyret @tt{List} to an array before processing it (and using JavaScript's
map/filter, etc.), since the Pyret version incurs more overhead.

@doc-internal["FFI" "isList" (list "Any") "JSBoolean"]

Returns @tt{true} if the value is a Pyret @pyret-id["List" "lists"] and
@tt{false} otherwise.

@section{Other Data Helpers}

@doc-internal["FFI" "makeSome" (list "Any") "Option"]
@doc-internal["FFI" "makeNone" (list) "Option"]

Create instances of @pyret-id["none" "option"] and @pyret-id["some" "option"]
from @secref["option"].

@doc-internal["FFI" "makeLeft" (list "Any") "Either"]
@doc-internal["FFI" "makeRight" (list "Any") "Either"]

Create instances of @pyret-id["left" "either"] and @pyret-id["right" "either"]
from @secref["either"].

@doc-internal["FFI" "cases" (list "(Any -> JSBoolean)" "JSString" "Any" "Handlers") "Any"]

This call emulates the functionality of the @tt{cases} expression in a
JavaScript context.  It takes a predicate to check (usually a function like
@tt{is-List}), a name for the predicate being checked, a @tt{data} value, and
an object containing handlers for its variants.  For example:

@verbatim{
function sum(l) {
  cases(runtime.getField(lists, "is-List"), "List", l, {
    empty: function() { return "Empty"; },
    link: function(f, r) { return f + sum(r); }
  });
}
sum(runtime.ffi.makeList([1,2])) // is 3
}

The predicate check and name are solely for error reporting.

