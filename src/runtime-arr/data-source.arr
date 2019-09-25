# Data Source Pyret Definitions
provide *
provide-types *
import global as G
import option as O
include from O:
  type Option,
end
none = O.none
some = O.some
num-to-string = G.num-to-str
string-to-number = G.string-to-number
raise = G.raise
torepr = G.js-to-string
string-tolower = G.string-to-lower

data CellContent<A>:
  | c-empty
  | c-str(s :: String)
  | c-num(n :: Number)
  | c-bool(b :: Boolean)
  | c-custom(datum :: A)
end

# Commenting out annotations because phase0 tried and failed to read them
# (Contents, Column, Row -> Sanitized)
type Sanitizer#|<A,B>|# = (CellContent<Any>, String, Number -> Any)

type LoadedTable#|<A,B>|# = {
  RawArray<{String; Sanitizer#|<A,B>|#}>;
  RawArray<RawArray<CellContent<Any>>>
}

data DataSourceLoaderOption<A,B>:
  | sanitize-col(col :: String, sanitizer :: Sanitizer#|<A,B>|#)
end

type DataSourceLoader#|<A,B>|# = {
  load :: (RawArray<String>, RawArray<DataSourceLoaderOption#|<A,B>|#> -> LoadedTable#|<A,B>|#)
}

fun option-sanitizer<A, B>(val-sanitizer :: (CellContent<A>, Number, Number -> CellContent<B>)) -> (CellContent<A>, Number, Number -> Option<CellContent<B>>):
  lam(x :: CellContent<A>, col, row):
    cases(CellContent) x:
      | c-empty => none
      | else => some(val-sanitizer(x, col, row))
    end
  end
end

fun string-sanitizer<A>(x :: CellContent<A>, col :: Number, row :: Number) -> String:
  cases(CellContent) x:
    | c-empty => ""
    | c-str(s) => s
    | c-num(n) => num-to-string(n)
    | c-bool(b) => torepr(b)
    | c-custom(datum) => torepr(datum)
  end
end

fun num-sanitizer<A>(x :: CellContent<A>, col :: Number, row :: Number) -> Number:
  loc = 'column ' + num-to-string(col) + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-str(s) =>
      cases(Option) string-to-number(s):
        | none => raise('Cannot sanitize the string "' + s
              + '" at ' + loc + ' as a number')
        | some(n) => n
      end
    | c-num(n) => n
    | c-bool(b) => if b: 1 else: 0 end
    | c-custom(datum) => raise('Cannot sanitize the datum '
          + torepr(datum) + ' at ' + loc + ' as a number')
    | c-empty => raise('Cannot sanitize the empty cell at '
          + loc + ' as a number')
  end
end

fun bool-sanitizer<A>(x :: CellContent<A>, col :: Number, row :: Number) -> Boolean:
  loc = 'column ' + num-to-string(col) + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-bool(b) => b
    | c-num(n) =>
      ask:
        | n == 0 then: false
        | n == 1 then: true
        | otherwise: raise('Cannot sanitize the number '
              + num-to-string(n) + ' at ' + loc + ' as a boolean')
      end
    | c-str(s) =>
      ask:
        | string-tolower(s) == "true" then: true
        | string-tolower(s) == "false" then: false
        | otherwise: raise('Cannot sanitize the string "'
              + s + '" at ' + loc + ' as a boolean')
      end
    | c-custom(datum) => raise('Cannot sanitize the datum '
          + torepr(datum) + ' at ' + loc + ' as a boolean')
    | c-empty => raise('Cannot sanitize the empty cell at '
          + loc + ' as a boolean')
  end
end

fun strict-num-sanitizer<A>(x :: CellContent<A>, col :: Number, row :: Number) -> Number:
  loc = 'column ' + num-to-string(col) + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-str(s) =>
      cases(Option) string-to-number(s):
        | none => raise('Cannot sanitize the string "'
              + s + '" at ' + loc + ' as a number')
        | some(n) => n
      end
    | c-num(n) => n
    | c-bool(b) => raise('Cannot sanitize the boolean '
          + torepr(b) + ' at ' + loc + ' as a number in strict mode.')
    | c-custom(datum) => raise('Cannot sanitize the datum '
          + torepr(datum) + ' at ' + loc + ' as a number')
    | c-empty => raise('Cannot sanitize the empty cell at '
          + loc + ' as a number')
  end
end

fun strings-only<A>(x :: CellContent<A>, col :: Number, row :: Number) -> String:
  loc = 'column ' + num-to-string(col) + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-str(s) => s
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "the number " + num-to-string(n)
        | c-bool(b) => "the boolean " + torepr(b)
        | c-custom(datum) => "the datum " + torepr(datum)
        | c-empty => "the empty cell"
        | c-str(s) => raise("unreachable")
      end
      raise('Cannot sanitize ' + as-str + ' at '
          + loc + ' as a string')
  end
end

fun numbers-only<A>(x :: CellContent<A>, col :: Number, row :: Number) -> Number:
  loc = 'column ' + num-to-string(col) + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-num(n) => n
    | else =>
      as-str = cases(CellContent) x:
        | c-str(s) => "the string " + torepr(s)
        | c-bool(b) => "the boolean " + torepr(b)
        | c-custom(datum) => "the datum " + torepr(datum)
        | c-empty => "an empty cell"
        | c-num(n) => raise("unreachable")
      end
      raise('Cannot sanitize ' + as-str + ' at '
          + loc + ' as a number')
  end
end

fun booleans-only<A>(x :: CellContent<A>, col :: Number, row :: Number) -> Boolean:
  loc = 'column ' + num-to-string(col) + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-bool(b) => b
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "the number " + num-to-string(n)
        | c-str(s) => "the string " + torepr(s)
        | c-custom(datum) => "the datum " + torepr(datum)
        | c-empty => "an empty cell"
        | c-bool(b) => raise("unreachable")
      end
      raise('Cannot sanitize ' + as-str + ' at '
          + loc + ' as a boolean')
  end
end

fun empty-only<A>(x :: CellContent<A>, col :: Number, row :: Number) -> Option<A>:
  loc = 'column ' + num-to-string(col) + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-empty => none
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "number " + num-to-string(n)
        | c-str(s) => "string " + torepr(s)
        | c-bool(b) => "boolean " + torepr(b)
        | c-custom(datum) => "datum " + torepr(datum)
        | c-empty => raise("unreachable")
      end
      raise('Cannot sanitize the ' + as-str + ' at '
          + loc + ' as an empty cell')
  end
end