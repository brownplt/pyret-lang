import error as E
check "JS field name shouldn't crash Pyret":
  {a: 5}.__proto__ raises-satisfies E.is-field-not-found
  {a: 5}.valueOf raises-satisfies E.is-field-not-found
  {a: 5}.hasOwnProperty raises-satisfies E.is-field-not-found
  {a: 5}.toString raises-satisfies E.is-field-not-found
  {a: 5}.toLocaleString raises-satisfies E.is-field-not-found
  {a: 5}.isPrototypeOf raises-satisfies E.is-field-not-found
  {a: 5}.propertyIsEnumerable raises-satisfies E.is-field-not-found
  {a: 5}.constructor raises-satisfies E.is-field-not-found
  to-repr({valueOf: 5}) is "{valueOf: 5}"
  to-repr({toString: "not a function"}) is '{toString: "not a function"}'
  to-repr({hasOwnProperty: "not a function"}) is '{hasOwnProperty: "not a function"}'
  to-repr({toLocaleString: "not a function"}) is '{toLocaleString: "not a function"}'
  to-repr({isPrototypeOf: "not a function"}) is '{isPrototypeOf: "not a function"}'
  to-repr({propertyIsEnumerable: "not a function"}) is '{propertyIsEnumerable: "not a function"}'
  to-repr({constructor: "not a function"}) is '{constructor: "not a function"}'
end

check "Object extension should still work":
  {a: 5}.{b: 6} is {a: 5, b: 6}
  {a: 5}.{b: 6}.{c: 7, a: 8} is {a: 8, b: 6, c: 7}
end
