({
  requires: [ ],
  provides: {
    shorthands: {
      "AnyPred":  ["arrow", ["Any"], "Boolean"],
      "AnyPred2":  ["arrow", ["Any", "Any"], "Boolean"],
      "NumPred":  ["arrow", ["Number"], "Boolean"],
      "NumPred2": ["arrow", ["Number", "Number"], "Boolean"],
      "NumBinop": ["arrow", ["Number", "Number"], "Number"],
      "NumUnop":  ["arrow", ["Number"], "Number"],
      "StrPred":  ["arrow", ["String"], "Boolean"],
      "StrPred2": ["arrow", ["String", "String"], "Boolean"],
      "StrBinop": ["arrow", ["String", "String"], "String"],
      "StrUnop":  ["arrow", ["String"], "String"],
    },
    values: {
      "nothing": "Nothing",
      "torepr": ["arrow", ["Any"], "String"],
      "tostring": ["arrow", ["Any"], "String"],
      "not": ["arrow", ["Boolean"], "Boolean"],

      "is-nothing": "AnyPred",
      "is-number": "AnyPred",
      "is-string": "AnyPred",
      "is-boolean": "AnyPred",
      "is-object": "AnyPred",
      "is-function": "AnyPred",
      "is-raw-array": "AnyPred",

      "equal-always": "AnyPred2",
      "equal-now": "AnyPred2",
      "identical": "AnyPred2",
      "within": ["arrow", ["Number"], "AnyPred2"],
      "within-abs": ["arrow", ["Number"], "AnyPred2"],
      "within-rel": ["arrow", ["Number"], "AnyPred2"],
      "within-now": ["arrow", ["Number"], "AnyPred2"],
      "within-abs-now": ["arrow", ["Number"], "AnyPred2"],
      "within-rel-now": ["arrow", ["Number"], "AnyPred2"],

      // Number functions

      "string-to-number": ["arrow", ["String"], ["Option", "Number"]],
      "num-is-integer": "NumPred",
      "num-is-rational": "NumPred",
      "num-is-roughnum": "NumPred",
      "num-is-positive": "NumPred",
      "num-is-negative": "NumPred",
      "num-is-non-positive": "NumPred",
      "num-is-non-negative": "NumPred",
      "num-is-fixnum": "NumPred",

      "num-min": "NumBinop",
      "num-max": "NumBinop",
      "num-equal": "NumPred2",
      "num-within": ["arrow", ["Number"], "NumPred2"],
      "num-within-abs": ["arrow", ["Number"], "NumPred2"],
      "num-within-rel": ["arrow", ["Number"], "NumPred2"],

      "num-abs": "NumUnop",
      "num-sin": "NumUnop",
      "num-cos": "NumUnop",
      "num-tan": "NumUnop",
      "num-asin": "NumUnop",
      "num-acos": "NumUnop",
      "num-atan": "NumUnop",

      "num-modulo": "NumBinop",

      "num-truncate": "NumUnop",
      "num-sqrt": "NumUnop",
      "num-sqr": "NumUnop",
      "num-ceiling": "NumUnop",
      "num-floor": "NumUnop",
      "num-round": "NumUnop",
      "num-round-even": "NumUnop",
      "num-log": "NumUnop",
      "num-exp": "NumUnop",
      "num-exact": "NumUnop",
      "num-to-rational": "NumUnop",
      "num-to-roughnum": "NumUnop",
      "num-to-fixnum": "NumUnop",

      "num-expt": "NumBinop",
      "num-tostring": ["arrow", ["Number"], "String"],
      "num-to-string": ["arrow", ["Number"], "String"],
      "num-to-string-digits": ["arrow", ["Number", "Number"], "String"],

      "random": "NumUnop",
      "num-random": "NumUnop",
      "num-random-seed": ["arrow", ["Number"], "Nothing"],

      // Time functions

      "time-now": ["arrow", [], "Number"],

      // String functions

      "gensym": ["arrow", [], "String"],
      "string-repeat": ["arrow", ["String", "Number"], "String"],
      "string-substring": ["arrow", ["String", "Number", "Number"], "String"],
      "string-toupper": "StrUnop",
      "string-tolower": "StrUnop",
      "string-append": "StrBinop",
      "string-equal": "StrPred2",
      "string-contains": "StrPred2",
      "string-length": ["arrow", ["String"], "Number"],
      "string-replace": ["arrow", ["String", "String", "String"], "String"],
      "string-char-at": ["arrow", ["String", "Number"], "String"],
      "string-to-code-point": ["arrow", ["String"], "Number"],
      "string-from-code-point": ["arrow", ["Number"], "String"],

    },
    aliases: {
      "Any": "tany"
    },
    datatypes: {
      "Number": ["data", "Number", [], [], {}],
      "Exactnum": ["data", "Exactnum", [], [], {}],
      "Roughnum": ["data", "Roughnum", [], [], {}],
      "NumInteger": ["data", "NumInteger", [], [], {}],
      "NumRational": ["data", "NumRational", [], [], {}],
      "NumPositive": ["data", "NumPositive", [], [], {}],
      "NumNegative": ["data", "NumNegative", [], [], {}],
      "NumNonPositive": ["data", "NumNonPositive", [], [], {}],
      "NumNonNegative": ["data", "NumNonNegative", [], [], {}],
      "String": ["data", "String", [], [], {}],
      "Function": ["data", "Function", [], [], {}],
      "Boolean": ["data", "Boolean", [], [], {}],
      "Object": ["data", "Object", [], [], {}],
      "Method": ["data", "Method", [], [], {}],
      "Nothing": ["data", "Nothing", [], [], {}],
      "RawArray": ["data", "RawArray", ["a"], [], {}]
    }
  },
  nativeRequires: [ ],
  theModule: function(runtime, namespace, uri /* intentionally blank */) {
    return runtime.globalModuleObject;
  }
})
