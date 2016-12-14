({
  requires: [ ],
  provides: {
    shorthands: {
      "AnyPred":  ["arrow", ["Any"], "Boolean"],
      "AnyPred2": ["arrow", ["Any", "Any"], "Boolean"],
      "NumPred":  ["arrow", ["Number"], "Boolean"],
      "NumPred2": ["arrow", ["Number", "Number"], "Boolean"],
      "NumBinop": ["arrow", ["Number", "Number"], "Number"],
      "NumUnop":  ["arrow", ["Number"], "Number"],
      "StrPred":  ["arrow", ["String"], "Boolean"],
      "StrPred2": ["arrow", ["String", "String"], "Boolean"],
      "StrBinop": ["arrow", ["String", "String"], "String"],
      "StrUnop":  ["arrow", ["String"], "String"],
      "tva":      ["tid", "a"],
      "tvb":      ["tid", "b"],
      "tvc":      ["tid", "c"],
      "tvd":      ["tid", "d"],
      "tve":      ["tid", "e"],
      "Equality": { tag: "name", 
                    origin: { "import-type": "uri", uri: "builtin://equality" },
                    name: "EqualityResult" }
    },
    values: {
      "nothing": "Nothing",
      "torepr": ["arrow", ["Any"], "String"],
      "to-repr": ["arrow", ["Any"], "String"],
      "tostring": ["arrow", ["Any"], "String"],
      "to-string": ["arrow", ["Any"], "String"],
      "not": ["arrow", ["Boolean"], "Boolean"],

      "is-nothing": "AnyPred",
      "is-number": "AnyPred",
      "is-string": "AnyPred",
      "is-boolean": "AnyPred",
      "is-object": "AnyPred",
      "is-function": "AnyPred",
      "is-raw-array": "AnyPred",
      "is-table": "AnyPred",
      "is-tuple": "AnyPred",

      // Array functions
      "raw-array":           ["forall", ["a"], ["Maker", "tva", ["RawArray", "tva"]]],
      "raw-array-get":       ["forall", ["a"], ["arrow", [["RawArray", "tva"], "Number"], "tva"]],
      "raw-array-set":       ["forall", ["a"], ["arrow", [["RawArray", "tva"], "Number", "tva"], 
                                                ["RawArray", "tva"]]],
      "raw-array-of":        ["forall", ["a"], ["arrow", ["tva", "Number"], ["RawArray", "tva"]]],
      "raw-array-build-opt": ["forall", ["a"], ["arrow", [["arrow", ["Number"], ["Option", "tva"]], "Number"],
                                                ["RawArray", "tva"]]],
      "raw-array-build":     ["forall", ["a"], ["arrow", [["arrow", ["Number"], "tva"], "Number"],
                                                ["RawArray", "tva"]]],
      "raw-array-length":    ["forall", ["a"], ["arrow", [["RawArray", "tva"]], "Number"]],
      "raw-array-to-list":   ["forall", ["a"], ["arrow", [["RawArray", "tva"]], ["List", "tva"]]],
      "raw-array-filter":    ["forall", ["a"], ["arrow", [["arrow", ["tva"], "Boolean"], ["RawArray", "tva"]], ["RawArray", "tva"]]],
      "raw-array-map":    ["forall", ["a", "b"], ["arrow", [["arrow", ["tva"], "tvb"], ["RawArray", "tva"]], ["RawArray", "tvb"]]],
      "raw-array-fold":      ["forall", ["a", "b"], ["arrow", [["arrow", ["tvb", "tva", "Number"], "tvb"], 
                                                               "tvb", ["RawArray", "tva"], "Number"], "tvb"]],

      // Equality functions

      "equal-always3": ["arrow", ["Any", "Any"], "Equality"],
      "equal-now3":    ["arrow", ["Any", "Any"], "Equality"],
      "identical3":    ["arrow", ["Any", "Any"], "Equality"],
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
      "num-atan2": "NumBinop",

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
      "string-to-upper": "StrUnop",
      "string-tolower": "StrUnop",
      "string-to-lower": "StrUnop",

      //"string-append": "StrBinop",
      "string-append": {
        tag: "v-fun",
        typ: "StrBinop",
        flatness: 0
      },
      "string-equal": "StrPred2",
      "string-contains": "StrPred2",
      "string-isnumber": "StrPred",
      "string-is-number": "StrPred",
      "string-to-number": ["arrow", ["String"], ["Option", "Number"]],
      "string-length": ["arrow", ["String"], "Number"],
      "string-replace": ["arrow", ["String", "String", "String"], "String"],
      "string-char-at": ["arrow", ["String", "Number"], "String"],
      "string-to-code-point": ["arrow", ["String"], "Number"],
      "string-from-code-point": ["arrow", ["Number"], "String"],
      "string-to-code-points": ["arrow", ["String"], ["List", "Number"]],
      "string-from-code-points": ["arrow", [["List", "Number"]], "String"],
      "string-split": ["arrow", ["String", "String"], ["List", "String"]],
      "string-split-all": ["arrow", ["String", "String"], ["List", "String"]],
      "string-explode": ["arrow", ["String"], ["List", "String"]],
      "string-index-of": ["arrow", ["String", "String"], "Number"],

    },
    aliases: {
      "Any": "tany"
    },
    datatypes: {
      "Number": ["data", "Number", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "Exactnum": ["data", "Exactnum", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "Roughnum": ["data", "Roughnum", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "NumInteger": ["data", "NumInteger", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "NumRational": ["data", "NumRational", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "NumPositive": ["data", "NumPositive", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "NumNegative": ["data", "NumNegative", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "NumNonPositive": ["data", "NumNonPositive", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "NumNonNegative": ["data", "NumNonNegative", [], [], {
        "_plus": ["arrow", ["Number"], "Number"],
        "_times": ["arrow", ["Number"], "Number"],
        "_divide": ["arrow", ["Number"], "Number"],
        "_minus": ["arrow", ["Number"], "Number"],
        "_lessthan": ["arrow", ["Number"], "Boolean"],
        "_lessequal": ["arrow", ["Number"], "Boolean"],
        "_greaterthan": ["arrow", ["Number"], "Boolean"],
        "_greaterequal": ["arrow", ["Number"], "Boolean"]
      }],
      "String": ["data", "String", [], [], {
        "_plus": ["arrow", ["String"], "String"],
        "_lessthan": ["arrow", ["String"], "Boolean"],
        "_lessequal": ["arrow", ["String"], "Boolean"],
        "_greaterthan": ["arrow", ["String"], "Boolean"],
        "_greaterequal": ["arrow", ["String"], "Boolean"]
      }],
      "Table": ["data", "Table", [], [], {
        "length": ["arrow", [], "Number"]
      }],
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
