#lang pyret

provide {
  file-name: file-name,
  args: other-args,
  Number: read-number,
  Bool: read-bool,
  String: read-string,
  Custom: read-custom,
  ParseParam: ParseParam,
  ParamRepeat: ParamRepeat,
  Param: Param,
  parse-args: parse-args,
  parse-cmdline: parse-cmdline,
  usage-info: usage-info,
  flag: flag,
  equals-val: equals-val,
  equals-val-default: equals-val-default,
  next-val: next-val,
  next-val-default: next-val-default,
  once: once,
  many: many,
  left: left,
  right: right,
  required-once: required-once,
  required-many: required-many,
  ParsedArguments: ParsedArguments,
  is-success: is-success,
  is-arg-error: is-arg-error
} end

import cmdline-lib as CL
import format as F
import string-dict as D
format = F.format

all-cmdline-params = CL.command-line-arguments()
file-name = all-cmdline-params.first
other-args = all-cmdline-params.rest

data Either:
  | left(val)
  | right(val)
end

data ParseParam:
  | read-number with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> Number:
      n = s.tonumber()
      if is-nothing(n):
        right(format("~a expected a numeric argument, got ~a", [param-name, torepr(s)]))
      else: left(n)
      end
    end,
    parse-string(self): "<number>" end
  | read-bool with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> Bool:
      if s == "true": left(true)
      else if s == "false": left(false)
      else:
        right(format("~a expected a boolean argument, got ~a", [param-name, torepr(s)]))
      end
    end,
    parse-string(self): "(true|false)" end
  | read-string with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> String:
      left(s)
    end,
    parse-string(self): "<string>" end
  | read-custom(name :: String, parser :: Function) with:
    parse(self, arg-index :: Number, param-name :: String, s :: String):
      self.parser(arg-index, param-name, s)
    end,
    parse-string(self): format("<~a>", self.name) end
end

data ParsedArguments:
  | success(parsed :: D.StringDict, unknown :: List<String>)
  | arg-error(message :: String, partial-results :: ParsedArguments)
end


data ParamRepeat:
  | once with: tostring(_): "may be used at most once" end
  | many with: tostring(_): "may be repeated" end
  | required-once with: tostring(_): "must be used exactly once" end
  | required-many with: tostring(_): "must be used at least once" end
end

data Param:
  | flag(repeated :: ParamRepeat, desc :: String)
  | equals-val(parser :: ParseParam, repeated :: ParamRepeat, desc :: String)
  | equals-val-default(
      parser :: ParseParam, default :: Any, short-name :: Option<String>, repeated :: ParamRepeat, desc :: String)
  | next-val(parser :: ParseParam, repeated :: ParamRepeat, desc :: String)
  | next-val-default(
      parser :: ParseParam, default :: Any, short-name :: Option<String>, repeated :: ParamRepeat, desc :: String)
end

# options : Dictionary of Params
fun usage-info(options-raw) -> List<String>:
  options = D.to-dict(options-raw)
  option-info = 
    for list.map(key from options.keys()):
      cases(Param) options.get(key):
        | flag(repeated, desc) =>
          format("  -~a: ~a (~a)", [key, desc, repeated])
        | equals-val(parser, repeated, desc) =>
          format("  --~a=~a: ~a (~a)", [key, parser.parse-string(), desc, repeated])
        | equals-val-default(parser, default, short-name, repeated, desc) =>
          cases(Option<String>) short-name:
            | none =>
              format("  --~a[=~a]: ~a (~a, default: ~a)", [key, parser.parse-string(), desc, repeated, default])
            | some(short) =>
              format("  --~a[=~a]: ~a (~a, default: ~a)\n  -~a: Defaults for ~a (~a)",
                [key, parser.parse-string(), desc, repeated, default, short, desc, repeated])
          end
        | next-val(parser, repeated, desc) =>
          format("  --~a ~a: ~a (~a)", [key, parser.parse-string(), desc, repeated])
        | next-val-default(parser, default, short-name, repeated, desc) =>
          cases(Option<String>) short-name:
            | none =>
              format("  --~a [~a]: ~a (~a, default: ~a)", [key, parser.parse-string(), desc, repeated, default])
            | some(short) =>
              format("  --~a [~a]: ~a (~a, default: ~a)\n  -~a: Defaults for ~a (~a)",
                [key, parser.parse-string(), desc, repeated, default, short, desc, repeated])
          end
      end
    end
  format("Usage: ~a [options] where:", [file-name]) ^ link(option-info)
end

# options : Dictionary of Params
# returns Dictionary where names are same as names of options, values are parsed values (if present)
fun parse-args(options, args :: List<String>) -> ParsedArguments:
  doc: 'Takes a dictionary of Param definitions, and a list of string arguments, 
and returns either the parsed argument results, or an error if the provided 
arguments do not satisfy the requirements of the Params dictionary.'
  opts-dict = D.to-dict(options)
  options-and-aliases =
    for list.fold(acc from {options: opts-dict, aliases: D.immutable-string-dict()}, key from opts-dict.keys()):
      if is-arg-error(acc): acc
      else:
        cur-option = opts-dict.get(key)
        cases(Param) cur-option:
          | equals-val-default(_, _, short-name, _, _) =>
            cases(Option<String>) short-name:
              | none => acc
              | some(short) =>
                if acc.options.has-key(short):
                  arg-error("Options map already includes entry for short-name " + short, success(D.immutable-string-dict(), []))
                else: acc.{options: acc.options, aliases: acc.aliases.set(short, key)}
                end
            end
          | next-val-default(_, _, short-name, _, _) =>
            cases(Option<String>) short-name:
              | none => acc
              | some(short) =>
                if acc.options.has-key(short):
                  arg-error("Options map already includes entry for short-name " + short, success(D.immutable-string-dict(), []))
                else: acc.{options: acc.options, aliases: acc.aliases.set(short, key)}
                end
            end
          | else => acc
        end
      end
    end
  if is-arg-error(options-and-aliases): options-and-aliases
  else:
    full-options = options-and-aliases.options
    option-aliases = options-and-aliases.aliases
    fun handle-repeated(results, repeated, name, val):
      cases(ParsedArguments) results:
        | success(parsed, unknown) =>
          cases(ParamRepeat) repeated:
            | once =>
              if results.parsed.has-key(name):
                arg-error(format("Parsing command line options for ~a failed: Option ~a ~a, and it has already been used", [file-name, name, repeated]), results)
              else: success(parsed.set(name, val), unknown)
              end
            | many =>
              if results.parsed.has-key(name):
                success(parsed.set(name, results.parsed.get(name) + [val]), unknown)
              else:
                success(parsed.set(name, [val]), unknown)
              end
            | required-once =>
              if results.parsed.has-key(name):
                arg-error(format("Parsing command line options for ~a failed: Option ~a ~a, and it has already been used", [file-name, name, repeated]), results)
              else: success(parsed.set(name, val), unknown)
              end
            | required-many =>
              if results.parsed.has-key(name):
                success(parsed.set(name, results.parsed.get(name) + [val]), unknown)
              else:
                success(parsed.set(name, [val]), unknown)
              end
          end
        | else => results
      end
    end
    required = for list.filter(key from opts-dict.keys()):
      repeated = opts-dict.get(key).repeated
      (repeated == required-once) or (repeated == required-many)
    end
    fun process(results, cur-index, remaining):
      if is-arg-error(results): results
      else:
        cases(List<String>) remaining:
          | empty => results
          | link(first, more-args) =>
            if builtins.string-substring(first, 0, 2) == "--":
              key-parts =
                builtins.string-split(
                  builtins.string-substring(first, 2, builtins.string-length(first)), "=", false)
              key = key-parts.first
              if full-options.has-key(key):
                cases(Param) full-options.get(key):
                  | equals-val(parser, repeated, _) =>
                    cases(List<String>) key-parts.rest:
                      | empty =>
                        arg-error(
                          format("Option ~a must be of the form --~a=~a", [key, key, parser.parse-string()]),
                          results)
                      | link(val, _) =>
                        parsed-val = parser.parse(cur-index, key, val)
                        cases(Either) parsed-val:
                          | left(v) => process(handle-repeated(results, repeated, key, v), cur-index + 1, more-args)
                          | right(e) => arg-error(e, results)
                        end
                    end
                  | equals-val-default(parser, default, _, repeated, _) =>
                    cases(List<String>) key-parts.rest:
                      | empty =>
                        process(handle-repeated(results, repeated, key, default), cur-index + 1, more-args)
                      | link(val, _) =>
                        parsed-val = parser.parse(cur-index, key, val)
                        cases(Either) parsed-val:
                          | left(v) => process(handle-repeated(results, repeated, key, v), cur-index + 1, more-args)
                          | right(e) => arg-error(e, results)
                        end
                    end
                  | next-val(parser, repeated, _) =>
                    cases(List<String>) key-parts.rest:
                      | empty =>
                        cases(List<String>) more-args:
                          | empty =>
                            arg-error(format("Missing value for option ~a; it must be of the form --~a ~a",
                                [key, key, parser.parse-string()]),
                              results)
                          | link(val, rest) =>
                            if builtins.string-char-at(val, 0) == "-":
                              parsed-val = parser.parse(cur-index, key, val)
                              cases(Either) parsed-val:
                                | left(v) => process(handle-repeated(results, repeated, key, v), cur-index + 2, rest)
                                | right(_) =>
                                  arg-error(format("Missing value for option ~a; it must be of the form --~a ~a",
                                      [key, key, parser.parse-string()]),
                                    results)
                              end
                            else:
                              parsed-val = parser.parse(cur-index + 1, key, val)
                              cases(Either) parsed-val:
                                | left(v) => process(handle-repeated(results, repeated, key, v), cur-index + 2, rest)
                                | right(e) => arg-error(e, results)
                              end
                            end
                        end
                      | else =>
                        arg-error(
                          format("Command line option --~a must be of the form --~a ~a, not --~a=~a",
                            [key, key, parser.parse-string(), key, parser.parse-string()]),
                          results)
                    end
                  | next-val-default(parser, default, _, repeated, _) =>
                    cases(List<String>) key-parts.rest:
                      | empty =>
                        cases(List<String>) more-args:
                          | empty => handle-repeated(results, repeated, key, default)
                          | link(val, rest) =>
                            if builtins.string-char-at(val, 0) == "-":
                              parsed-val = parser.parse(cur-index, key, val)
                              cases(Either) parsed-val:
                                | left(v) =>
                                  process(handle-repeated(results, repeated, key, v), cur-index + 2, rest)
                                | right(e) =>
                                  process(handle-repeated(results, repeated, key, default), cur-index + 1, more-args)
                              end
                            else:
                              parsed-val = parser.parse(cur-index, key, val)
                              cases(Either) parsed-val:
                                | left(v) => process(handle-repeated(results, repeated, key, v), cur-index + 1, rest)
                                | right(e) => arg-error(e, results)
                              end
                            end
                        end
                      | else =>
                        arg-error(format("Command line option --~a must be of the form --~a ~a, not --~a=~a",
                            [key, key, parser.parse-string(), key, parser.parse-string()]),
                          results)
                    end
                  | else =>
                    arg-error(format("Command line option -~a does not start with two dashes", [key]), results)
                end
              else:
                arg-error("Unknown command line option --" + key, results)
              end
            else if builtins.string-substring(first, 0, 1) == "-":
              key = builtins.string-substring(first, 1, builtins.string-length(first))
              lookup = 
                if option-aliases.has-key(key) and full-options.has-key(option-aliases.get(key)):
                  full-options.get(option-aliases.get(key))
                else if full-options.has-key(key):
                  full-options.get(key)
                else:
                  nothing
                end
              if Param(lookup):
                cases(Param) lookup:
                  | flag(repeated, _) =>
                    process(handle-repeated(results, repeated, key, true), cur-index + 1, more-args)
                  | equals-val-default(_, default, _, repeated, _) =>
                    process(handle-repeated(results, repeated, option-aliases.get(key), default), cur-index + 1, more-args)
                  | next-val-default(_, default, _, repeated, _) =>
                    process(handle-repeated(results, repeated, option-aliases.get(key), default), cur-index + 1, more-args)
                  | else => arg-error(format("Command line option --~a must start with two dashes", [key]), results)
                end
              else:
                arg-error("Unknown command line option -" + key, results)
              end
            else:
              process(success(results.parsed, results.unknown + [first]), cur-index + 1, more-args)
            end
        end
      end
    end
    parsed-results = process(success(D.immutable-string-dict(), []), 1, args)
    cases(ParsedArguments) parsed-results:
      | success(parsed, other) =>
        filled-missing-defaults = for list.fold(acc from parsed, key from opts-dict.keys()):
          cases(Param) opts-dict.get(key):
            | next-val-default(_, default, _, repeated, _) =>
              if (not acc.has-key(key)) and ((repeated == once) or (repeated == many)): acc.set(key, default)
              else: acc
              end
            | equals-val-default(_, default, _, repeated, _) =>
              if (not acc.has-key(key)) and ((repeated == once) or (repeated == many)): acc.set(key, default)
              else: acc
              end
            | else => acc
          end              
        end
        missing-args = for list.filter(key from required):
          not filled-missing-defaults.has-key(key)
        end
        if is-empty(missing-args): success(filled-missing-defaults, other)
        else:
          arg-error(
            format("Command line option validation for ~a failed: The following options are required but not found: ~a",
              [file-name, missing-args]), parsed-results)
        end
      | else => parsed-results
    end
  end
end

fun parse-cmdline(options):
  doc: 'Parses the actual command line arguments against the provided options dictionary'
  parse-args(options, other-args)
end


fun dict(l):
  for fold(d from D.immutable-string-dict(), i from list.range(0, l.length() / 2)):
    d.set(l.get(2 * i), l.get((2 * i) + 1))
  end
end


check:
  fun error-text(msg): fun(val):
      cases(ParsedArguments) val:
        | success(_, _) => false
        | arg-error(m, _) => m.contains(msg)
      end
  end end
  
  once-optional-flag = {
    foo: flag(once, "Foo")
  }
  parse-args(once-optional-flag, ["-foo"]) is success(dict(["foo", true]), [])
  parse-args(once-optional-flag, ["bar"]) is success(D.immutable-string-dict(), ["bar"])
  parse-args(once-optional-flag, ["--foo"]) satisfies error-text("two dashes")
  parse-args(once-optional-flag, ["-foo", "-foo"]) satisfies error-text("already been used")
  parse-args(once-optional-flag, ["-foo", "bar"]) is success(dict(["foo", true]), ["bar"])
  parse-args(once-optional-flag, ["bar", "-foo"]) is success(dict(["foo", true]), ["bar"])
  parse-args(once-optional-flag, ["-bar"]) satisfies error-text("Unknown command line option -bar")
  parse-args(once-optional-flag, ["--bar"]) satisfies error-text("Unknown command line option --bar")

  once-required-flag = {
    foo: flag(required-once, "Foo")
  }
  parse-args(once-required-flag, ["-foo"]) is success(dict(["foo", true]), [])
  parse-args(once-required-flag, ["bar"]) satisfies error-text("options are required")
  parse-args(once-required-flag, ["--foo"]) satisfies error-text("two dashes")
  parse-args(once-required-flag, ["-foo", "-foo"]) satisfies error-text("already been used")
  parse-args(once-required-flag, ["-foo", "bar"]) is success(dict(["foo", true]), ["bar"])
  parse-args(once-required-flag, ["bar", "-foo"]) is success(dict(["foo", true]), ["bar"])

  
  once-required-equals-default = {
    foo: equals-val-default(read-number, 42, some("f"), required-once, "Foo"),
    bar: flag(once, "Bar")
  }
  parse-args(once-required-equals-default, ["--foo=3"]) is success(dict(["foo", 3]), [])
  parse-args(once-required-equals-default, ["--foo=bar"]) satisfies error-text("expected a numeric argument")
  parse-args(once-required-equals-default, ["--foo=3", "--foo=4"]) satisfies error-text("already been used")
  parse-args(once-required-equals-default, ["-f"]) is success(dict(["foo", 42]), [])
  parse-args(once-required-equals-default, ["--foo"]) is success(dict(["foo", 42]), [])
  parse-args(once-required-equals-default, ["--foo", "-bar"]) is success(dict(["foo", 42, "bar", true]), [])
  parse-args(once-required-equals-default, ["-bar", "--foo"]) is success(dict(["foo", 42, "bar", true]), [])
  parse-args(once-required-equals-default, ["-bar", "-f"]) is success(dict(["foo", 42, "bar", true]), [])


  once-optional-next-default = {
    width: next-val-default(read-number, 80, some("w"), once, "Width")
  }
  parse-args(once-optional-next-default, ["-w", "foo.txt"]) is success(dict(["width", 80]), ["foo.txt"])
  parse-args(once-optional-next-default, ["--width", "120", "foo.txt"]) is success(dict(["width", 120]), ["foo.txt"])
  parse-args(once-optional-next-default, ["foo.txt"]) is success(dict(["width", 80]), ["foo.txt"])
  parse-args(once-optional-next-default, ["--w", "120", "foo.txt"]) satisfies error-text("Unknown command line option --w")
  
  once-required-next-default = {
    foo: next-val-default(read-number, 42, some("f"), required-once, "Foo"),
    bar: flag(once, "Bar")
  }
  parse-args(once-required-next-default, ["--foo", "3"]) is success(dict(["foo", 3]), [])
  parse-args(once-required-next-default, ["--foo", "bar"]) satisfies error-text("expected a numeric argument")
  parse-args(once-required-next-default, ["--foo", "3", "--foo", "4"]) satisfies error-text("already been used")
  parse-args(once-required-next-default, ["-f"]) is success(dict(["foo", 42]), [])
  parse-args(once-required-next-default, ["--foo"]) is success(dict(["foo", 42]), [])
  parse-args(once-required-next-default, ["--foo", "-bar"]) is success(dict(["foo", 42, "bar", true]), [])
  parse-args(once-required-next-default, ["-f", "-bar"]) is success(dict(["foo", 42, "bar", true]), [])
  parse-args(once-required-next-default, ["-bar", "-f"]) is success(dict(["foo", 42, "bar", true]), [])

  many-optional-flag = {
    foo: flag(many, "Foo"),
    bar: next-val-default(read-number, 42, some("b"), many, "Bar"),
    ["4"]: flag(many, "Flag-4")
  }
  parse-args(many-optional-flag, ["-foo", "-foo", "-foo"]) is success(dict(["foo", [true, true, true]]), [])
  parse-args(many-optional-flag, ["-b", "-foo", "--bar", "3", "--bar", "-foo"])
    is success(dict(["foo", [true, true], "bar", [42, 3, 42]]), [])
  parse-args(many-optional-flag, ["-b", "-foo", "-b", "3", "--bar", "-foo"])
    is success(dict(["foo", [true, true], "bar", [42, 42, 42]]), ["3"])
  parse-args(many-optional-flag, ["--bar", "-4"]) is success(dict(["bar", [-4]]), [])
  parse-args(many-optional-flag, ["--bar", "-not-a-number"]) satisfies error-text("Unknown command line option -not-a-number")
  parse-args(many-optional-flag, ["--bar", "-4"]) is success(dict(["bar", [-4]]), [])
  parse-args(many-optional-flag, ["--bar", "-4", "-4"]) is success(dict(["bar", [-4], "4", [true]]), [])

  many-required-equals = {
    foo: equals-val(read-bool, required-many, "Foo"),
    bar: flag(many, "Bar")
  }
  parse-args(many-required-equals, ["--foo=false", "--foo=true"]) is success(dict(["foo", [false, true]]), [])
  parse-args(many-required-equals, ["-bar"]) satisfies error-text("options are required")
  parse-args(many-required-equals, ["--foo"]) satisfies error-text("Option foo must be of the form --foo=(true|false)")

  many-required-next-str = {
    foo: next-val(read-string, required-many, "Foo"),
    bar: flag(many, "Bar")
  }
  parse-args(many-required-next-str, ["--foo", "-bar"]) is success(dict(["foo", ["-bar"]]), [])
  parse-args(many-required-next-str, ["-bar", "--foo"]) satisfies error-text("Missing value for option foo; it must be of the form --foo <string>")

  many-required-next-num = {
    foo: next-val(read-number, required-many, "Foo"),
    bar: flag(many, "Bar"),
    ["4"]: flag(many, "Flag-4")
  }
  parse-args(many-required-next-num, ["--foo", "-bar"]) satisfies error-text("Missing value for option foo; it must be of the form --foo <number>")
  parse-args(many-required-next-num, ["--foo", "-4"]) is success(dict(["foo", [-4]]), [])
  parse-args(many-required-next-num, ["--foo", "-4", "-4"]) is success(dict(["foo", [-4], "4", [true]]), [])

  data RGB: red | green | blue end
  custom-parser = read-custom("red|green|blue", fun(arg-index, name, val):
      if val == "red": left(red)
      else if val == "green": left(green)
      else if val == "blue": left(blue)
      else: right(format("~a expected an RGB argument, got ~a", [name, torepr(val)]))
      end
    end)
  many-next-colors = {
    color: next-val-default(custom-parser, red, some("c"), many, "Color")
  }
  parse-args(many-next-colors, ["--color", "red"]) is success(dict(["color", [red]]), [])
  parse-args(many-next-colors, ["--color", "red", "-c", "--color", "blue"])
    is success(dict(["color", [red, red, blue]]), [])
  parse-args(many-next-colors, ["--color", "bad"]) satisfies error-text("color expected an RGB argument, got \"bad\"")
  parse-args(many-next-colors, ["--color", "green", "--color", "-c", "blue"])
    is success(dict(["color", [green, red, red]]), ["blue"])
end


