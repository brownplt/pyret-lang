define(["js/namespace", "trove/image", "trove/world", "js/js-numbers"], function(Namespace, imageLib, worldLib, jsnums) {

  function makeBootstrapNamespace(rt) {
    return rt.loadModules(rt.namespace, [imageLib, worldLib], function(image, world) {
      var get = rt.getField;

      var makeFunction = rt.makeFunction;

      /* Most of what Bootstrap uses is just friendly names for the identifiers
       * we are used to (e.g. add, sub, max, instead of num-add, num-sub,
       * num-max).  A few, like the boolean functions and, or, and not, are
       * special to Bootstrap and we define them here. */

      var bool_and = function(l, r) {
        rt.ffi.checkArity(2, arguments, "both");
        rt.checkBoolean(l);
        rt.checkBoolean(r);
        return rt.makeBoolean(l && r);
      }
      var bool_or = function(l, r) {
        rt.ffi.checkArity(2, arguments, "either");
        rt.checkBoolean(l);
        rt.checkBoolean(r);
        return rt.makeBoolean(l || r);
      }

      var add = function(l, r) {
        rt.ffi.checkArity(2, arguments, "add");
        rt.checkNumber(l);
        rt.checkNumber(r);
        return jsnums.add(l, r);
      }
      var sub = function(l, r) {
        rt.ffi.checkArity(2, arguments, "sub");
        rt.checkNumber(l);
        rt.checkNumber(r);
        return jsnums.subtract(l, r);
      }
      var mul = function(l, r) {
        rt.ffi.checkArity(2, arguments, "mul");
        rt.checkNumber(l);
        rt.checkNumber(r);
        return jsnums.multiply(l, r);
      }
      var div = function(l, r) {
        rt.ffi.checkArity(2, arguments, "div");
        rt.checkNumber(l);
        rt.checkNumber(r);
        return jsnums.divide(l, r);
      }
      var n = Namespace.namespace({
        '_empty': rt.namespace.get("_empty"),
        '_plus': rt.namespace.get("_plus"),
        '_minus': rt.namespace.get("_minus"),
        '_times': rt.namespace.get("_times"),
        '_divide': rt.namespace.get("_divide"),
        '_lessthan': rt.namespace.get("_lessthan"),
        '_greaterthan': rt.namespace.get("_greaterthan"),
        '_greaterequal': rt.namespace.get("_greaterequal"),
        '_lessequal': rt.namespace.get("_lessequal"),

        'torepr': rt.namespace.get("torepr"),
        'tostring': rt.namespace.get("tostring"),
        'print': rt.namespace.get("print"),
        'display': rt.namespace.get("display"),
        'print-error': rt.namespace.get("print-error"),
        'display-error': rt.namespace.get("display-error"),
        'raise': rt.namespace.get("raise"),
        'builtins': rt.namespace.get("builtins"),
        'nothing': rt.namespace.get("nothing"),
        'is-nothing': rt.namespace.get("is-nothing"),
        'is-number': rt.namespace.get("is-number"),
        'is-boolean': rt.namespace.get("is-boolean"),
        'is-string': rt.namespace.get("is-string"),
        'is-function': rt.namespace.get("is-function"),
        'is-object': rt.namespace.get("is-object"),

        'add': rt.makeFunction(add),
        'sub': rt.makeFunction(sub),
        'mul': rt.makeFunction(mul),
        'div': rt.makeFunction(div),
        'less': rt.namespace.get("_lessthan"),
        'greater': rt.namespace.get("_greaterthan"),
        'greaterequal': rt.namespace.get("_greaterequal"),
        'lessequal': rt.namespace.get("_lessequal"),

        'both': makeFunction(bool_and),
        'either': makeFunction(bool_or),
        'not': rt.namespace.get("not"),

        'max': rt.namespace.get("num-max"),
        'min': rt.namespace.get("num-min"),

        'random': rt.namespace.get("random"),

        'within-now': rt.namespace.get("within-now"),
        'within-rel-now': rt.namespace.get("within-rel-now"),
        'within': rt.namespace.get("within"),
        'within-rel': rt.namespace.get("within-rel"),

        'num-max': rt.namespace.get("num-max"),
        'num-min': rt.namespace.get("num-min"),
        'num-equal': rt.namespace.get("num-equal"),
        'num-within': rt.namespace.get("num-within"),
        'num-within-rel': rt.namespace.get("num-within-rel"),
        'num-abs': rt.namespace.get("num-abs"),
        'num-sin': rt.namespace.get("num-sin"),
        'num-cos': rt.namespace.get("num-cos"),
        'num-tan': rt.namespace.get("num-tan"),
        'num-asin': rt.namespace.get("num-asin"),
        'num-acos': rt.namespace.get("num-acos"),
        'num-atan': rt.namespace.get("num-atan"),
        'num-modulo': rt.namespace.get("num-modulo"),
        'num-truncate': rt.namespace.get("num-truncate"),
        'num-sqrt': rt.namespace.get("num-sqrt"),
        'num-sqr': rt.namespace.get("num-sqr"),
        'num-ceiling': rt.namespace.get("num-ceiling"),
        'num-floor': rt.namespace.get("num-floor"),
        'num-round': rt.namespace.get("num-round"),
        'num-log': rt.namespace.get("num-log"),
        'num-exp': rt.namespace.get("num-exp"),
        'num-exact': rt.namespace.get("num-exact"),
        'num-is-integer': rt.namespace.get("num-is-integer"),
        'num-is-rational': rt.namespace.get("num-is-rational"),
        'num-is-roughnum': rt.namespace.get("num-is-roughnum"),
        'num-is-positive': rt.namespace.get("num-is-positive"),
        'num-is-negative': rt.namespace.get("num-is-negative"),
        'num-is-non-positive': rt.namespace.get("num-is-non-positive"),
        'num-is-non-negative': rt.namespace.get("num-is-non-negative"),
        'num-expt': rt.namespace.get("num-expt"),
        'num-tostring': rt.namespace.get("num-tostring"),
        'max': rt.namespace.get("num-max"),
        'min': rt.namespace.get("num-min"),
        'abs': rt.namespace.get("num-abs"),
        'sin': rt.namespace.get("num-sin"),
        'cos': rt.namespace.get("num-cos"),
        'tan': rt.namespace.get("num-tan"),
        'asin': rt.namespace.get("num-asin"),
        'acos': rt.namespace.get("num-acos"),
        'atan': rt.namespace.get("num-atan"),
        'modulo': rt.namespace.get("num-modulo"),
        'truncate': rt.namespace.get("num-truncate"),
        'sqrt': rt.namespace.get("num-sqrt"),
        'sqr': rt.namespace.get("num-sqr"),
        'ceiling': rt.namespace.get("num-ceiling"),
        'floor': rt.namespace.get("num-floor"),
        'round': rt.namespace.get("num-round"),
        'log': rt.namespace.get("num-log"),
        'exp': rt.namespace.get("num-exp"),
        'exact': rt.namespace.get("num-exact"),
        'is-integer': rt.namespace.get("num-is-integer"),
        'is-fixnum': rt.namespace.get("num-is-fixnum"),
        'expt': rt.namespace.get("num-expt"),

        'strings-equal': rt.namespace.get("strings-equal"),
        'string-contains': rt.namespace.get("string-contains"),
        'string-append': rt.namespace.get("string-append"),
        'string-length': rt.namespace.get("string-length"),
        'string-tonumber': rt.namespace.get("string-tonumber"),
        'string-repeat': rt.namespace.get("string-repeat"),
        'string-substring': rt.namespace.get("string-substring"),
        'string-replace': rt.namespace.get("string-replace"),
        'string-split': rt.namespace.get("string-split"),
        'string-char-at': rt.namespace.get("string-char-at"),
        'string-toupper': rt.namespace.get("string-toupper"),
        'string-tolower': rt.namespace.get("string-tolower"),
        'string-explode': rt.namespace.get("string-explode"),
        'string-index-of': rt.namespace.get("string-index-of"),

        'Number': rt.namespace.get("Number"),
        'String': rt.namespace.get("String"),
        'Function': rt.namespace.get("Function"),
        'Boolean': rt.namespace.get("Boolean"),
        'Object': rt.namespace.get("Object"),
        'Method': rt.namespace.get("Method"),
        'Nothing': rt.namespace.get("Nothing"),
        'RawArray': rt.namespace.get("RawArray")

      });
      var imageFields = {};
      rt.getFields(image).forEach(function(f) {
        imageFields[f] = get(image, f);
      });
      var worldFields = {};
      rt.getFields(world).forEach(function(f) {
        worldFields[f] = get(world, f);
      });
      return n.merge(Namespace.namespace(imageFields)).merge(Namespace.namespace(worldFields));
    });
  }

  return {
    create: makeBootstrapNamespace
  };
});
