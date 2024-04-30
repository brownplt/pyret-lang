# Data Source Pyret Definitions
provide *
provide-types *
import global as _
include option

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

fun option-sanitizer(val-sanitizer):
  lam(x, col, row):
    cases(CellContent) x:
      | c-empty => none
      | else => some(val-sanitizer(x, col, row))
    end
  end
end

fun string-sanitizer(x, col, row):
  cases(CellContent) x:
    | c-empty => ""
    | c-str(s) => s
    | c-num(n) => num-to-string(n)
    | c-bool(b) => torepr(b)
    | c-custom(datum) => torepr(datum)
  end
end

fun num-sanitizer(x, col, row):
  loc = 'column ' + col + ', row ' + num-to-string(row)
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

fun bool-sanitizer(x, col, row):
  loc = 'column ' + col + ', row ' + num-to-string(row)
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

fun strict-num-sanitizer(x, col, row):
  loc = 'column ' + col + ', row ' + num-to-string(row)
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

fun strings-only(x, col, row):
  loc = 'column ' + col + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-str(s) => s
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "the number " + num-to-string(n)
        | c-bool(b) => "the boolean " + torepr(b)
        | c-datum(datum) => "the datum " + torepr(datum)
        | c-empty => "the empty cell"
      end
      raise('Cannot sanitize ' + as-str + ' at '
          + loc + ' as a string')
  end
end

fun numbers-only(x, col, row):
  loc = 'column ' + col + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-num(n) => n
    | else =>
      as-str = cases(CellContent) x:
        | c-str(s) => "the string " + torepr(s)
        | c-bool(b) => "the boolean " + torepr(b)
        | c-datum(datum) => "the datum " + torepr(datum)
        | c-empty => "an empty cell"
      end
      raise('Cannot sanitize ' + as-str + ' at '
          + loc + ' as a number')
  end
end

fun booleans-only(x, col, row):
  loc = 'column ' + col + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-bool(b) => b
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "the number " + num-to-string(n)
        | c-str(s) => "the string " + torepr(s)
        | c-datum(datum) => "the datum " + torepr(datum)
        | c-empty => "an empty cell"
      end
      raise('Cannot sanitize ' + as-str + ' at '
          + loc + ' as a boolean')
  end
end

fun empty-only(x, col, row):
  loc = 'column ' + col + ', row ' + num-to-string(row)
  cases(CellContent) x:
    | c-empty => none
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "number " + num-to-string(n)
        | c-str(s) => "string " + torepr(s)
        | c-bool(b) => "boolean " + torepr(b)
        | c-datum(datum) => "datum " + torepr(datum)
      end
      raise('Cannot sanitize the ' + as-str + ' at '
          + loc + ' as an empty cell')
  end
end