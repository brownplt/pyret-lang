# Data Source Pyret Definitions
provide *
provide-types *
import global as _
import option as O

data CellContent<A>:
  | c-empty
  | c-str(s :: String)
  | c-num(n :: Number)
  | c-bool(b :: Boolean)
  | c-custom(datum :: A)
end

# Commenting out annotations because phase0 tried and failed to read them
type Sanitizer#|<A,B>|# = (CellContent<Any> -> Any)

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
  lam(x):
    cases(CellContent) x:
      | c-empty => O.none
      | else => O.some(val-sanitizer(x))
    end
  end
end

fun string-sanitizer(x):
  cases(CellContent) x:
    | c-empty => ""
    | c-str(s) => s
    | c-num(n) => num-to-string(n)
    | c-bool(b) => torepr(b)
    | c-custom(datum) => torepr(datum)
  end
end

fun num-sanitizer(x):
  cases(CellContent) x:
    | c-str(s) =>
      cases(O.Option) string-to-number(s):
        | none => raise('Cannot sanitize the string "' + s + '" as a number')
        | some(n) => n
      end
    | c-num(n) => n
    | c-bool(b) => if b: 1 else: 0 end
    | c-custom(datum) => raise('Cannot sanitize the datum '
          + torepr(datum) + ' as a number')
    | c-empty => raise('Cannot sanitize an empty cell as a number')
  end
end

fun bool-sanitizer(x):
  cases(CellContent) x:
    | c-bool(b) => b
    | c-num(n) =>
      ask:
        | n == 0 then: false
        | n == 1 then: true
        | otherwise: raise('Cannot sanitize the number '
              + num-to-string(n) + ' as a boolean')
      end
    | c-str(s) =>
      ask:
        | string-tolower(s) == "true" then: true
        | string-tolower(s) == "false" then: false
        | otherwise: raise('Cannot sanitize the string "'
              + s + '" as a boolean')
      end
    | c-custom(datum) => raise('Cannot sanitize the datum '
          + torepr(datum) + ' as a boolean')
    | c-empty => raise('Cannot sanitize an empty cell as a boolean')
  end
end

fun strict-num-sanitizer(x):
  cases(CellContent) x:
    | c-str(s) =>
      cases(O.Option) string-to-number(s):
        | none => raise('Cannot sanitize the string "' + s + '" as a number')
        | some(n) => n
      end
    | c-num(n) => n
    | c-bool(b) => raise('Cannot sanitize the boolean '
          + torepr(b) + ' as a number in strict mode.')
    | c-custom(datum) => raise('Cannot sanitize the datum '
          + torepr(datum) + ' as a number')
    | c-empty => raise('Cannot sanitize an empty cell as a number')
  end
end

fun strings-only(x):
  cases(CellContent) x:
    | c-str(s) => s
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "the number " + num-to-string(n)
        | c-bool(b) => "the boolean " + torepr(b)
        | c-datum(datum) => "the datum " + torepr(datum)
        | c-empty => "an empty cell"
      end
      raise('Cannot sanitize ' + as-str + ' as a string')
  end
end

fun numbers-only(x):
  cases(CellContent) x:
    | c-num(n) => n
    | else =>
      as-str = cases(CellContent) x:
        | c-str(s) => "the string " + torepr(s)
        | c-bool(b) => "the boolean " + torepr(b)
        | c-datum(datum) => "the datum " + torepr(datum)
        | c-empty => "an empty cell"
      end
      raise('Cannot sanitize ' + as-str + ' as a number')
  end
end

fun booleans-only(x):
  cases(CellContent) x:
    | c-bool(b) => b
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "the number " + num-to-string(n)
        | c-str(s) => "the string " + torepr(s)
        | c-datum(datum) => "the datum " + torepr(datum)
        | c-empty => "an empty cell"
      end
      raise('Cannot sanitize ' + as-str + ' as a boolean')
  end
end

fun empty-only(x):
  cases(CellContent) x:
    | c-empty => O.none
    | else =>
      as-str = cases(CellContent) x:
        | c-num(n) => "number " + num-to-string(n)
        | c-str(s) => "string " + torepr(s)
        | c-bool(b) => "boolean " + torepr(b)
        | c-datum(datum) => "datum " + torepr(datum)
      end
      raise('Cannot sanitize the ' + as-str + ' as an empty cell')
  end
end