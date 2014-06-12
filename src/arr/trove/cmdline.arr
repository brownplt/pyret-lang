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
#provide-types *

import cmdline-lib as CL
import format as F
import string-dict as D
import either as E
format = F.format
Either = E.Either
left = E.left
right = E.right

all-cmdline-params = CL.command-line-arguments()
file-name = all-cmdline-params.first
other-args = all-cmdline-params.rest

data ParseParam:
  | read-number with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> Number:
      n = string-tonumber(s)
      if is-nothing(n):
        right(format("~a expected a numeric argument, got ~a", [list: param-name, torepr(s)]))
      else: left(n)
      end
    end,
    parse-string(self): "<number>" end
  | read-bool with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> Boolean:
      if s == "true": left(true)
      else if s == "false": left(false)
      else:
        right(format("~a expected a boolean argument, got ~a", [list: param-name, torepr(s)]))
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
    parse-string(self): format("<~a>", [list: self.name]) end
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

fun is-Param_(l):
  is-flag(l) or is-equals-val(l) or is-equals-val-default(l) or
    is-next-val(l) or is-next-val-default(l)
end

# options : Dictionary of Params
fun usage-info(options-raw) -> List<String>:
  options = D.to-dict(options-raw)
  option-info = 
    for lists.map(key from options.keys()):
      cases(Param) options.get(key):
        | flag(repeated, desc) =>
          format("  -~a: ~a (~a)", [list: key, desc, repeated])
        | equals-val(parser, repeated, desc) =>
          format("  --~a=~a: ~a (~a)", [list: key, parser.parse-string(), desc, repeated])
        | equals-val-default(parser, default, short-name, repeated, desc) =>
          cases(Option<String>) short-name:
            | none =>
              format("  --~a[list: =~a]: ~a (~a, default: ~a)", [list: key, parser.parse-string(), desc, repeated, default])
            | some(short) =>
              format("  --~a[list: =~a]: ~a (~a, default: ~a)\n  -~a: Defaults for ~a (~a)",
                [list: key, parser.parse-string(), desc, repeated, default, short, desc, repeated])
          end
        | next-val(parser, repeated, desc) =>
          format("  --~a ~a: ~a (~a)", [list: key, parser.parse-string(), desc, repeated])
        | next-val-default(parser, default, short-name, repeated, desc) =>
          cases(Option<String>) short-name:
            | none =>
              format("  --~a [list: ~a]: ~a (~a, default: ~a)", [list: key, parser.parse-string(), desc, repeated, default])
            | some(short) =>
              format("  --~a [list: ~a]: ~a (~a, default: ~a)\n  -~a: Defaults for ~a (~a)",
                [list: key, parser.parse-string(), desc, repeated, default, short, desc, repeated])
          end
      end
    end
  format("Usage: ~a [list: options] where:", [list: file-name]) ^ link(_, option-info)
end

# options : Dictionary of Params
# returns Dictionary where names are same as names of options, values are parsed values (if present)
fun parse-args(options, args :: List<String>) -> ParsedArguments:
  doc: ```Takes a dictionary of Param definitions, and a list of string arguments, 
  and returns either the parsed argument results, or an error if the provided 
  arguments do not satisfy the requirements of the Params dictionary.```
  opts-dict = D.to-dict(options)
  options-and-aliases =
    for lists.fold(acc from {options: opts-dict, aliases: D.immutable-string-dict()}, key from opts-dict.keys()):
      if is-arg-error(acc): acc
      else:
        cur-option = opts-dict.get(key)
        cases(Param) cur-option:
          | equals-val-default(_, _, short-name, _, _) =>
            cases(Option<String>) short-name:
              | none => acc
              | some(short) =>
                if acc.options.has-key(short):
                  arg-error("Options map already includes entry for short-name " + short, success(D.immutable-string-dict(), [list: ]))
                else: acc.{options: acc.options, aliases: acc.aliases.set(short, key)}
                end
            end
          | next-val-default(_, _, short-name, _, _) =>
            cases(Option<String>) short-name:
              | none => acc
              | some(short) =>
                if acc.options.has-key(short):
                  arg-error("Options map already includes entry for short-name " + short, success(D.immutable-string-dict(), [list: ]))
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
                arg-error(format("Parsing command line options for ~a failed: Option ~a ~a, and it has already been used", [list: file-name, name, repeated]), results)
              else: success(parsed.set(name, val), unknown)
              end
            | many =>
              if results.parsed.has-key(name):
                success(parsed.set(name, results.parsed.get(name) + [list: val]), unknown)
              else:
                success(parsed.set(name, [list: val]), unknown)
              end
            | required-once =>
              if results.parsed.has-key(name):
                arg-error(format("Parsing command line options for ~a failed: Option ~a ~a, and it has already been used", [list: file-name, name, repeated]), results)
              else: success(parsed.set(name, val), unknown)
              end
            | required-many =>
              if results.parsed.has-key(name):
                success(parsed.set(name, results.parsed.get(name) + [list: val]), unknown)
              else:
                success(parsed.set(name, [list: val]), unknown)
              end
          end
        | else => results
      end
    end
    required = for lists.filter(key from opts-dict.keys()):
      repeated = opts-dict.get(key).repeated
      (repeated == required-once) or (repeated == required-many)
    end
    fun process(results, cur-index, remaining):
      if is-arg-error(results): results
      else:
        cases(List<String>) remaining:
          | empty => results
          | link(first, more-args) =>
            if string-substring(first, 0, 2) == "--":
              key-parts = string-split(string-substring(first, 2, string-length(first)), "=", false)
              key = key-parts.first
              if full-options.has-key(key):
                cases(Param) full-options.get(key):
                  | equals-val(parser, repeated, _) =>
                    cases(List<String>) key-parts.rest:
                      | empty =>
                        arg-error(
                          format("Option ~a must be of the form --~a=~a", [list: key, key, parser.parse-string()]),
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
                                [list: key, key, parser.parse-string()]),
                              results)
                          | link(val, rest) =>
                            if string-char-at(val, 0) == "-":
                              parsed-val = parser.parse(cur-index, key, val)
                              cases(Either) parsed-val:
                                | left(v) => process(handle-repeated(results, repeated, key, v), cur-index + 2, rest)
                                | right(_) =>
                                  arg-error(format("Missing value for option ~a; it must be of the form --~a ~a",
                                      [list: key, key, parser.parse-string()]),
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
                            [list: key, key, parser.parse-string(), key, parser.parse-string()]),
                          results)
                    end
                  | next-val-default(parser, default, _, repeated, _) =>
                    cases(List<String>) key-parts.rest:
                      | empty =>
                        cases(List<String>) more-args:
                          | empty => handle-repeated(results, repeated, key, default)
                          | link(val, rest) =>
                            if string-char-at(val, 0) == "-":
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
                            [list: key, key, parser.parse-string(), key, parser.parse-string()]),
                          results)
                    end
                  | else =>
                    arg-error(format("Command line option -~a does not start with two dashes", [list: key]), results)
                end
              else:
                arg-error("Unknown command line option --" + key, results)
              end
            else if string-substring(first, 0, 1) == "-":
              key = string-substring(first, 1, string-length(first))
              lookup = 
                if option-aliases.has-key(key) and full-options.has-key(option-aliases.get(key)):
                  full-options.get(option-aliases.get(key))
                else if full-options.has-key(key):
                  full-options.get(key)
                else:
                  nothing
                end
              if is-Param_(lookup):
                cases(Param) lookup:
                  | flag(repeated, _) =>
                    process(handle-repeated(results, repeated, key, true), cur-index + 1, more-args)
                  | equals-val-default(_, default, _, repeated, _) =>
                    process(handle-repeated(results, repeated, option-aliases.get(key), default), cur-index + 1, more-args)
                  | next-val-default(_, default, _, repeated, _) =>
                    process(handle-repeated(results, repeated, option-aliases.get(key), default), cur-index + 1, more-args)
                  | else => arg-error(format("Command line option --~a must start with two dashes", [list: key]), results)
                end
              else:
                arg-error("Unknown command line option -" + key, results)
              end
            else:
              success(results.parsed, results.unknown + remaining) # STOP PROCESSING after first non-option value
            end
        end
      end
    end
    parsed-results = process(success(D.immutable-string-dict(), [list: ]), 1, args)
    cases(ParsedArguments) parsed-results:
      | success(parsed, other) =>
        filled-missing-defaults = for lists.fold(acc from parsed, key from opts-dict.keys()):
          cases(Param) opts-dict.get(key):
            | next-val-default(_, default, _, repeated, _) =>
              if not(acc.has-key(key)) and ((repeated == once) or (repeated == many)): acc.set(key, default)
              else: acc
              end
            | equals-val-default(_, default, _, repeated, _) =>
              if not(acc.has-key(key)) and ((repeated == once) or (repeated == many)): acc.set(key, default)
              else: acc
              end
            | else => acc
          end              
        end
        missing-args = for lists.filter(key from required):
          not(filled-missing-defaults.has-key(key))
        end
        if is-empty(missing-args): success(filled-missing-defaults, other)
        else:
          arg-error(
            format("Command line option validation for ~a failed: The following options are required but not found: ~a",
              [list: file-name, missing-args]), parsed-results)
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
  for fold(d from D.immutable-string-dict(), i from lists.range(0, l.length() / 2)):
    d.set(l.get(2 * i), l.get((2 * i) + 1))
  end
end


data RGB: red | green | blue end

check:
  fun error-text(msg): lam(val):
      cases(ParsedArguments) val:
        | success(_, _) => false
        | arg-error(m, _) => m.contains(msg)
      end
  end end
  
  once-optional-flag = {
    foo: flag(once, "Foo")
  }
  parse-args(once-optional-flag, [list: "-foo"]) is success(dict([list: "foo", true]), [list: ])
  parse-args(once-optional-flag, [list: "bar"]) is success(D.immutable-string-dict(), [list: "bar"])
  parse-args(once-optional-flag, [list: "--foo"]) satisfies error-text("two dashes")
  parse-args(once-optional-flag, [list: "-foo", "-foo"]) satisfies error-text("already been used")
  parse-args(once-optional-flag, [list: "-foo", "bar"]) is success(dict([list: "foo", true]), [list: "bar"])
  parse-args(once-optional-flag, [list: "bar", "-foo"]) is success(dict([list: "foo", true]), [list: "bar"])
  parse-args(once-optional-flag, [list: "-bar"]) satisfies error-text("Unknown command line option -bar")
  parse-args(once-optional-flag, [list: "--bar"]) satisfies error-text("Unknown command line option --bar")

  once-required-flag = {
    foo: flag(required-once, "Foo")
  }
  parse-args(once-required-flag, [list: "-foo"]) is success(dict([list: "foo", true]), [list: ])
  parse-args(once-required-flag, [list: "bar"]) satisfies error-text("options are required")
  parse-args(once-required-flag, [list: "--foo"]) satisfies error-text("two dashes")
  parse-args(once-required-flag, [list: "-foo", "-foo"]) satisfies error-text("already been used")
  parse-args(once-required-flag, [list: "-foo", "bar"]) is success(dict([list: "foo", true]), [list: "bar"])
  parse-args(once-required-flag, [list: "bar", "-foo"]) is success(dict([list: "foo", true]), [list: "bar"])

  
  once-required-equals-default = {
    foo: equals-val-default(read-number, 42, some("f"), required-once, "Foo"),
    bar: flag(once, "Bar")
  }
  parse-args(once-required-equals-default, [list: "--foo=3"]) is success(dict([list: "foo", 3]), [list: ])
  parse-args(once-required-equals-default, [list: "--foo=bar"]) satisfies error-text("expected a numeric argument")
  parse-args(once-required-equals-default, [list: "--foo=3", "--foo=4"]) satisfies error-text("already been used")
  parse-args(once-required-equals-default, [list: "-f"]) is success(dict([list: "foo", 42]), [list: ])
  parse-args(once-required-equals-default, [list: "--foo"]) is success(dict([list: "foo", 42]), [list: ])
  parse-args(once-required-equals-default, [list: "--foo", "-bar"]) is success(dict([list: "foo", 42, "bar", true]), [list: ])
  parse-args(once-required-equals-default, [list: "-bar", "--foo"]) is success(dict([list: "foo", 42, "bar", true]), [list: ])
  parse-args(once-required-equals-default, [list: "-bar", "-f"]) is success(dict([list: "foo", 42, "bar", true]), [list: ])


  once-optional-next-default = {
    width: next-val-default(read-number, 80, some("w"), once, "Width")
  }
  parse-args(once-optional-next-default, [list: "-w", "foo.txt"]) is success(dict([list: "width", 80]), [list: "foo.txt"])
  parse-args(once-optional-next-default, [list: "--width", "120", "foo.txt"]) is success(dict([list: "width", 120]), [list: "foo.txt"])
  parse-args(once-optional-next-default, [list: "foo.txt"]) is success(dict([list: "width", 80]), [list: "foo.txt"])
  parse-args(once-optional-next-default, [list: "--w", "120", "foo.txt"]) satisfies error-text("Unknown command line option --w")
  
  once-required-next-default = {
    foo: next-val-default(read-number, 42, some("f"), required-once, "Foo"),
    bar: flag(once, "Bar")
  }
  parse-args(once-required-next-default, [list: "--foo", "3"]) is success(dict([list: "foo", 3]), [list: ])
  parse-args(once-required-next-default, [list: "--foo", "bar"]) satisfies error-text("expected a numeric argument")
  parse-args(once-required-next-default, [list: "--foo", "3", "--foo", "4"]) satisfies error-text("already been used")
  parse-args(once-required-next-default, [list: "-f"]) is success(dict([list: "foo", 42]), [list: ])
  parse-args(once-required-next-default, [list: "--foo"]) is success(dict([list: "foo", 42]), [list: ])
  parse-args(once-required-next-default, [list: "--foo", "-bar"]) is success(dict([list: "foo", 42, "bar", true]), [list: ])
  parse-args(once-required-next-default, [list: "-f", "-bar"]) is success(dict([list: "foo", 42, "bar", true]), [list: ])
  parse-args(once-required-next-default, [list: "-bar", "-f"]) is success(dict([list: "foo", 42, "bar", true]), [list: ])

  many-optional-flag = {
    foo: flag(many, "Foo"),
    bar: next-val-default(read-number, 42, some("b"), many, "Bar"),
    ["4"]: flag(many, "Flag-4")
  }
  parse-args(many-optional-flag, [list: "-foo", "-foo", "-foo"]) is success(dict([list: "foo", [list: true, true, true]]), [list: ])
  parse-args(many-optional-flag, [list: "-b", "-foo", "--bar", "3", "--bar", "-foo"])
    is success(dict([list: "foo", [list: true, true], "bar", [list: 42, 3, 42]]), [list: ])
  parse-args(many-optional-flag, [list: "-b", "-foo", "-b", "3", "--bar", "-foo"])
    is success(dict([list: "foo", [list: true, true], "bar", [list: 42, 42, 42]]), [list: "3"])
  parse-args(many-optional-flag, [list: "--bar", "-4"]) is success(dict([list: "bar", [list: -4]]), [list: ])
  parse-args(many-optional-flag, [list: "--bar", "-not-a-number"]) satisfies error-text("Unknown command line option -not-a-number")
  parse-args(many-optional-flag, [list: "--bar", "-4"]) is success(dict([list: "bar", [list: -4]]), [list: ])
  parse-args(many-optional-flag, [list: "--bar", "-4", "-4"]) is success(dict([list: "bar", [list: -4], "4", [list: true]]), [list: ])

  many-required-equals = {
    foo: equals-val(read-bool, required-many, "Foo"),
    bar: flag(many, "Bar")
  }
  parse-args(many-required-equals, [list: "--foo=false", "--foo=true"]) is success(dict([list: "foo", [list: false, true]]), [list: ])
  parse-args(many-required-equals, [list: "-bar"]) satisfies error-text("options are required")
  parse-args(many-required-equals, [list: "--foo"]) satisfies error-text("Option foo must be of the form --foo=(true|false)")

  many-required-next-str = {
    foo: next-val(read-string, required-many, "Foo"),
    bar: flag(many, "Bar")
  }
  parse-args(many-required-next-str, [list: "--foo", "-bar"]) is success(dict([list: "foo", [list: "-bar"]]), [list: ])
  parse-args(many-required-next-str, [list: "-bar", "--foo"]) satisfies error-text("Missing value for option foo; it must be of the form --foo <string>")

  many-required-next-num = {
    foo: next-val(read-number, required-many, "Foo"),
    bar: flag(many, "Bar"),
    ["4"]: flag(many, "Flag-4")
  }
  parse-args(many-required-next-num, [list: "--foo", "-bar"]) satisfies error-text("Missing value for option foo; it must be of the form --foo <number>")
  parse-args(many-required-next-num, [list: "--foo", "-4"]) is success(dict([list: "foo", [list: -4]]), [list: ])
  parse-args(many-required-next-num, [list: "--foo", "-4", "-4"]) is success(dict([list: "foo", [list: -4], "4", [list: true]]), [list: ])

  custom-parser = read-custom("red|green|blue", lam(arg-index, name, val):
      if val == "red": left(red)
      else if val == "green": left(green)
      else if val == "blue": left(blue)
      else: right(format("~a expected an RGB argument, got ~a", [list: name, torepr(val)]))
      end
    end)
  many-next-colors = {
    color: next-val-default(custom-parser, red, some("c"), many, "Color")
  }
  parse-args(many-next-colors, [list: "--color", "red"]) is success(dict([list: "color", [list: red]]), [list: ])
  parse-args(many-next-colors, [list: "--color", "red", "-c", "--color", "blue"])
    is success(dict([list: "color", [list: red, red, blue]]), [list: ])
  parse-args(many-next-colors, [list: "--color", "bad"]) satisfies error-text("color expected an RGB argument, got \"bad\"")
  parse-args(many-next-colors, [list: "--color", "green", "--color", "-c", "blue"])
    is success(dict([list: "color", [list: green, red, red]]), [list: "blue"])
end


