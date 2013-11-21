#lang pyret

provide {
  file-name: file-name,
  args: other-args,
  Number: read-number,
  Bool: read-bool,
  String: read-string,
  Custom: read-custom,
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
  required-once: required-once,
  required-many: required-many,
} end

import Racket as R
import format as F
format = F.format

all-cmdline-params = R("pyret/parameters")("command-line-arguments")
file-name = all-cmdline-params.first
other-args = all-cmdline-params.rest

fun type-error(path, line, pos, message):
  error.make-error({path: path, line: line, column: pos,
      system: true, value: {type: "type-error", message: message}})
end

data ParseParam:
  | read-number with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> Number:
      n = s.tonumber()
      if is-nothing(n):
        raise(type-error(
            file-name, "Command line", "argument #" + tostring(arg-index),
            format("~a expected a numeric argument, got ~a", [param-name, torepr(s)])))
      else: n
      end
    end,
    parse-string(self): "<number>" end
  | read-bool with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> Bool:
      if s == "true": true
      else if s == "false": false
      else:
        raise(type-error(file-name, "Command line", "argument #" + tostring(arg-index),
            format("~a expected a boolean argument, got ~a", [param-name, torepr(s)])))
      end
    end,
    parse-string(self): "(true|false)" end
  | read-string with:
    parse(_, arg-index :: Number, param-name :: String, s :: String) -> String:
      s
    end,
    parse-string(self): "<string>" end
  | read-custom(name :: String, parser :: Function) with:
    parse(self, arg-index :: Number, param-name :: String, s :: String):
      self.parser(arg-index, param-name, s)
    end,
    parse-string(self): format("<~a>", self.name) end
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
fun usage-info(options) -> List<String>:
  option-info = 
    for list.map(key from builtins.keys(options)):
      cases(Param) options.[key]:
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
                [key, parser.parse-string(), desc, repeated, default, short, default, repeated])
          end
      end
    end
  format("Usage: ~a [options] where:", [file-name]) ^ link(option-info)
end

# options : Dictionary of Params
# returns Dictionary where names are same as names of options, values are parsed values (if present)
fun parse-args(options, args :: List<String>):
  options-and-aliases = for list.fold(acc from {options: options, aliases: {}}, key from builtins.keys(options)):
    cur-option = options.[key]
    cases(Param) cur-option:
      | equals-val-default(_, _, short-name, _, _) =>
        cases(Option<String>) short-name:
          | none => acc
          | some(short) =>
            if builtins.has-field(acc.options, short):
              raise(type-error(file-name, "Command line options", key,
                  "Options map already includes entry for short-name " + short))
            else: acc.{options: acc.options.{[short]: cur-option}, aliases: acc.aliases.{[short]: key}}
            end
        end
      | next-val-default(_, _, short-name, _, _) =>
        cases(Option<String>) short-name:
          | none => acc
          | some(short) =>
            if builtins.has-field(acc.options, short):
              raise(type-error(file-name, "Command line options", key,
                  "Options map already includes entry for short-name " + short))
            else: acc.{options: acc.options.{[short]: cur-option}, aliases: acc.aliases.{[short]: key}}
            end
        end
      | else => acc
    end
  end
  full-options = options-and-aliases.options
  option-aliases = options-and-aliases.aliases
  fun handle-repeated(results, repeated, name, val):
    cases(ParamRepeat) repeated:
      | once =>
        if builtins.has-field(results.parsed, name):
          raise(type-error(file-name, "Command line options", name,
              format("Option ~a ~a, and it has already been used", [name, repeated])))
        else: results.{parsed: results.parsed.{[name]: val}}
        end
      | many =>
        if builtins.has-field(results.parsed, name):
          results.{parsed: results.parsed.{[name]: results.parsed.[name] + [val]}}
        else:
          results.{parsed: results.parsed.{[name]: [val]}}
        end
      | required-once =>
        if builtins.has-field(results.parsed, name):
          raise(type-error(file-name, "Command line options", name,
              format("Option ~a ~a, and it has already been used", [name, repeated])))
        else: results.{parsed: results.parsed.{[name]: val}}
        end
      | required-many =>
        if builtins.has-field(results.parsed, name):
          results.{parsed: results.parsed.{[name]: results.parsed.[name] + [val]}}
        else:
          results.{parsed: results.parsed.{[name]: [val]}}
        end
    end
  end
  required = for list.filter(key from builtins.keys(options)):
    repeated = options.[key].repeated
    (repeated == required-once) or (repeated == required-many)
  end
  fun process(results, cur-index, remaining):
    cases(List<String>) remaining:
      | empty => results
      | link(first, more-args) =>
        if first.substring(0, 2) == "--":
          key-parts = first.substring(2, first.length()).split("=", false)
          key = key-parts.first
          if builtins.has-field(full-options, key):
            cases(Param) full-options.[key]:
              | equals-val(parser, repeated, _) =>
                cases(List<String>) key-parts.rest:
                  | empty =>
                    raise(type-error(file-name, "Command line options", key,
                        format("Option ~a must be of the form --~a=~a", [key, key, parser.parse-string()])))
                  | link(val, _) =>
                    process(
                      handle-repeated(results, repeated, key, parser.parse(cur-index, key, val)),
                      cur-index + 1, more-args)
                end
              | equals-val-default(parser, default, _, repeated, _) =>
                cases(List<String>) key-parts.rest:
                  | empty =>
                    process(handle-repeated(results, repeated, key, default), cur-index + 1, more-args)
                  | link(val, _) =>
                    process(
                      handle-repeated(results, repeated, key, parser.parse(cur-index, key, val)),
                      cur-index + 1, more-args)
                end
              | next-val(parser, repeated, _) =>
                cases(List<String>) key-parts.rest:
                  | empty =>
                    cases(List<String>) more-args:
                      | empty =>
                        raise(type-error(file-name, "Command line options", key,
                            format("Missing value for option ~a; it must be of the form --~a ~a",
                              [key, key, parser.parse-string()])))
                      | link(val, rest) =>
                        if val.char-at(0) == "-":
                          parsed-val = try:
                            parser.parse(cur-index, key, val)
                          except(e):
                            raise(type-error(file-name, "Command line options", key,
                                format("Missing value for option ~a; it must be of the form --~a ~a",
                                  [key, key, parser.parse-string()])))
                          end
                          process(handle-repeated(results, repeated, key, parsed-val), cur-index + 2, rest)
                        else:
                          process(
                            handle-repeated(results, repeated, key, parser.parse(cur-index + 1, key, val)),
                            cur-index + 2, rest)
                        end
                    end
                  | else =>
                    raise(type-error(file-name, "Command line options", key,
                        format("Command line option --~a must be of the form --~a ~a, not --~a=~a",
                          [key, key, parser.parse-string(), key, parser.parse-string()])))
                end
              | next-val-default(parser, default, _, repeated, _) =>
                cases(List<String>) key-parts.rest:
                  | empty =>
                    cases(List<String>) more-args:
                      | empty => handle-repeated(results, repeated, key, default)
                      | link(val, rest) =>
                        if val.char-at(0) == "-":
                          parsed-val = try:
                            some(parser.parse(cur-index, key, val))
                          except(e):
                            none
                          end
                          cases(Option<Any>) parsed-val:
                            | some(v) =>
                              process(handle-repeated(results, repeated, key, v), cur-index + 2, rest)
                            | none =>
                              process(handle-repeated(results, repeated, key, default), cur-index + 1, more-args)
                          end
                        else:
                          process(
                            handle-repeated(results, repeated, key, parser.parse(cur-index, key, val)),
                            cur-index + 1, rest)
                        end
                    end
                  | else =>
                    raise(type-error(file-name, "Command line options", key,
                        format("Command line option --~a must be of the form --~a ~a, not --~a=~a",
                          [key, key, parser.parse-string(), key, parser.parse-string()])))
                end
              | else => raise(type-error(file-name, "Command line options", key,
                    format("Command line option -~a does not start with two dashes", [key])))
            end
          else:
            raise(type-error(file-name, "Command line options", key,
                "Unknown command line option --" + key))
          end
        else if first.substring(0, 1) == "-":
          key = first.substring(1, first.length())
          if builtins.has-field(full-options, key):
            cases(Param) full-options.[key]:
              | flag(repeated, _) =>
                process(handle-repeated(results, repeated, key, true), cur-index + 1, more-args)
              | equals-val-default(_, default, _, repeated, _) =>
                process(handle-repeated(results, repeated, option-aliases.[key], default), cur-index + 1, more-args)
              | next-val-default(_, default, _, repeated, _) =>
                process(handle-repeated(results, repeated, option-aliases.[key], default), cur-index + 1, more-args)
              | else => raise(type-error(file-name, "Command line options", key,
                    format("Command line option --~a must start with two dashes", [key])))
            end
          else:
            raise(type-error(file-name, "Command line options", key,
                "Unknown command line option -" + key))
          end
        else:
          process(results.{unknown: results.unknown + [first]}, cur-index + 1, more-args)
        end
    end
  end
  parsed-results = process({parsed: {}, unknown: []}, 1, args)
  missing-args = for list.filter(key from required):
    not builtins.has-field(parsed-results.parsed, key)
  end
  if is-empty(missing-args): parsed-results
  else:
    raise(type-error(file-name, "Command line options", "validation",
        format("The following options are required but not found: ~a", [missing-args])))
  end
end
parse-cmdline = parse-args(_, other-args)



check:
  once-optional-flag = {
    foo: flag(once, "Foo")
  }
  parse-args(once-optional-flag, ["-foo"]) is {unknown: [], parsed: {foo: true}}
  parse-args(once-optional-flag, ["bar"]) is {unknown: ["bar"], parsed: {}}
  parse-args(once-optional-flag, ["--foo"]) raises "two dashes"
  parse-args(once-optional-flag, ["-foo", "-foo"]) raises "already been used"
  parse-args(once-optional-flag, ["-foo", "bar"]) is {unknown: ["bar"], parsed: {foo: true}}
  parse-args(once-optional-flag, ["bar", "-foo"]) is {unknown: ["bar"], parsed: {foo: true}}
  parse-args(once-optional-flag, ["-bar"]) raises "Unknown command line option -bar"
  parse-args(once-optional-flag, ["--bar"]) raises "Unknown command line option --bar"

  once-required-flag = {
    foo: flag(required-once, "Foo")
  }
  parse-args(once-required-flag, ["-foo"]) is {unknown: [], parsed: {foo: true}}
  parse-args(once-required-flag, ["bar"]) raises "options are required"
  parse-args(once-required-flag, ["--foo"]) raises "two dashes"
  parse-args(once-required-flag, ["-foo", "-foo"]) raises "already been used"
  parse-args(once-required-flag, ["-foo", "bar"]) is {unknown: ["bar"], parsed: {foo: true}}
  parse-args(once-required-flag, ["bar", "-foo"]) is {unknown: ["bar"], parsed: {foo: true}}

  
  once-required-equals-default = {
    foo: equals-val-default(read-number, 42, some("f"), required-once, "Foo"),
    bar: flag(once, "Bar")
  }
  parse-args(once-required-equals-default, ["--foo=3"]) is {unknown: [], parsed: {foo: 3}}
  parse-args(once-required-equals-default, ["--foo=bar"]) raises "expected a numeric argument"
  parse-args(once-required-equals-default, ["--foo=3", "--foo=4"]) raises "already been used"
  parse-args(once-required-equals-default, ["-f"]) is {unknown: [], parsed: {foo: 42}}
  parse-args(once-required-equals-default, ["--foo"]) is {unknown: [], parsed: {foo: 42}}
  parse-args(once-required-equals-default, ["--foo", "-bar"]) is {unknown: [], parsed: {foo: 42, bar: true}}
  parse-args(once-required-equals-default, ["-bar", "--foo"]) is {unknown: [], parsed: {foo: 42, bar: true}}
  parse-args(once-required-equals-default, ["-bar", "-f"]) is {unknown: [], parsed: {foo: 42, bar: true}}

  once-required-next-default = {
    foo: next-val-default(read-number, 42, some("f"), required-once, "Foo"),
    bar: flag(once, "Bar")
  }
  parse-args(once-required-next-default, ["--foo", "3"]) is {unknown: [], parsed: {foo: 3}}
  parse-args(once-required-next-default, ["--foo", "bar"]) raises "expected a numeric argument"
  parse-args(once-required-next-default, ["--foo", "3", "--foo", "4"]) raises "already been used"
  parse-args(once-required-next-default, ["-f"]) is {unknown: [], parsed: {foo: 42}}
  parse-args(once-required-next-default, ["--foo"]) is {unknown: [], parsed: {foo: 42}}
  parse-args(once-required-next-default, ["--foo", "-bar"]) is {unknown: [], parsed: {foo: 42, bar: true}}
  parse-args(once-required-next-default, ["-f", "-bar"]) is {unknown: [], parsed: {foo: 42, bar: true}}
  parse-args(once-required-next-default, ["-bar", "-f"]) is {unknown: [], parsed: {foo: 42, bar: true}}

  many-optional-flag = {
    foo: flag(many, "Foo"),
    bar: next-val-default(read-number, 42, some("b"), many, "Bar"),
    ["4"]: flag(many, "Flag-4")
  }
  parse-args(many-optional-flag, ["-foo", "-foo", "-foo"]) is {unknown: [], parsed: {foo: [true, true, true]}}
  parse-args(many-optional-flag, ["-b", "-foo", "--bar", "3", "--bar", "-foo"]) is {unknown: [],
    parsed: {foo: [true, true], bar: [42, 3, 42]}}
  parse-args(many-optional-flag, ["-b", "-foo", "-b", "3", "--bar", "-foo"]) is {unknown: ["3"],
    parsed: {foo: [true, true], bar: [42, 42, 42]}}
  parse-args(many-optional-flag, ["--bar", "-4"]) is {unknown: [], parsed: {bar: [-4]}}
  parse-args(many-optional-flag, ["--bar", "-not-a-number"]) raises "Unknown command line option -not-a-number"
  parse-args(many-optional-flag, ["--bar", "-4"]) is {unknown: [], parsed: {bar: [-4]}}
  parse-args(many-optional-flag, ["--bar", "-4", "-4"]) is {unknown: [], parsed: {bar: [-4], ["4"]: [true]}}

  many-required-equals = {
    foo: equals-val(read-bool, required-many, "Foo"),
    bar: flag(many, "Bar")
  }
  parse-args(many-required-equals, ["--foo=false", "--foo=true"]) is {unknown: [], parsed: {foo: [false, true]}}
  parse-args(many-required-equals, ["-bar"]) raises "options are required"
  parse-args(many-required-equals, ["--foo"]) raises "Option foo must be of the form --foo=(true|false)"

  many-required-next-str = {
    foo: next-val(read-string, required-many, "Foo"),
    bar: flag(many, "Bar")
  }
  parse-args(many-required-next-str, ["--foo", "-bar"]) is {unknown: [], parsed: {foo: ["-bar"]}}
  parse-args(many-required-next-str, ["-bar", "--foo"]) raises "Missing value for option foo; it must be of the form --foo <string>"

  many-required-next-num = {
    foo: next-val(read-number, required-many, "Foo"),
    bar: flag(many, "Bar"),
    ["4"]: flag(many, "Flag-4")
  }
  parse-args(many-required-next-num, ["--foo", "-bar"]) raises "Missing value for option foo; it must be of the form --foo <number>"
  parse-args(many-required-next-num, ["--foo", "-4"]) is {unknown: [], parsed: {foo: [-4]}}
  parse-args(many-required-next-num, ["--foo", "-4", "-4"]) is {unknown: [], parsed: {foo: [-4], ["4"]: [true]}}

  data RGB: red | green | blue end
  custom-parser = read-custom("red|green|blue", fun(arg-index, name, val):
      if val == "red": red
      else if val == "green": green
      else if val == "blue": blue
      else: raise(type-error(file-name, "Command line", "argument #" + tostring(arg-index),
            format("~a expected an RGB argument, got ~a", [name, torepr(val)])))
      end
    end)
  many-next-colors = {
    color: next-val-default(custom-parser, red, some("c"), many, "Color")
  }
  parse-args(many-next-colors, ["--color", "red"]) is {unknown: [], parsed: {color: [red]}}
  parse-args(many-next-colors, ["--color", "red", "-c", "--color", "blue"])
    is {unknown: [], parsed: {color: [red, red, blue]}}
  parse-args(many-next-colors, ["--color", "bad"]) raises "color expected an RGB argument, got \"bad\""
  parse-args(many-next-colors, ["--color", "green", "--color", "-c", "blue"])
    is {unknown: ["blue"], parsed: {color: [green, red, red]}}
end
