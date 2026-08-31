// Port of src/arr/trove/cmdline.arr
//
// Option-parsing semantics are ported line by line; in particular:
// - boolean flags are matched with a SINGLE leading dash (-no-check-mode);
//   value options use TWO dashes (--outfile val). A flag given with two
//   dashes produces "Command line option -<key> does not start with two
//   dashes"; a value option given with one dash produces "Command line
//   option --<key> must start with two dashes".
// - processing STOPS at the first non-option argument; that argument and
//   everything after it are returned as `unknown`.
// - Pyret's string-dict iteration is hash-ordered; here `options` is a
//   Map and iteration is insertion-ordered, so usage-info line ORDER can
//   differ from the Pyret CLI (per-line text is identical).

import { Either, left, right, mapGetValue, mapSet } from './shared';
import { jsnums, throwingErrbacks } from './interop/js-numbers';

// cmdline-lib: command-line-arguments() = process.argv.slice(1)
// (handalone.js sets the runtime param exactly this way; Node resolves
// argv[1] to an absolute path, which is what the Pyret CLI observes too).
export const allCmdlineParams: string[] = process.argv.slice(1);
export const fileName: string =
  allCmdlineParams.length > 0 ? allCmdlineParams[0] : '<browser>';
export const otherArgs: string[] = allCmdlineParams.slice(1);

// ---------- small rendering helpers (Pyret torepr/tostring on the few ----------
// ---------- value shapes that appear in cmdline messages)              ----------

// torepr of a Pyret string (used in parse error messages)
export function toreprString(s: string): string {
  let out = '"';
  for (const ch of s) {
    switch (ch) {
      case '"': out += '\\"'; break;
      case '\\': out += '\\\\'; break;
      case '\n': out += '\\n'; break;
      case '\t': out += '\\t'; break;
      case '\r': out += '\\r'; break;
      default: out += ch;
    }
  }
  return out + '"';
}

// torepr/tostring of a List<String> ("[list: \"a\", \"b\"]")
export function stringListToRepr(items: string[]): string {
  if (items.length === 0) { return '[list: ]'; }
  return '[list: ' + items.map(toreprString).join(', ') + ']';
}

// format's ~a (tostring): strings render raw, everything else via String()
function toDisplay(v: any): string {
  if (typeof v === 'string') { return v; }
  return String(v);
}

// ---------- data ParseParam ----------

export abstract class ParseParamBase {
  abstract get $name(): string;
  abstract parse(argIndex: number, paramName: string, s: string): Either<any, string>;
  abstract parseString(): string;
}

export class ReadNumber extends ParseParamBase {
  get $name(): 'read-number' { return 'read-number'; }
  parse(_argIndex: number, paramName: string, s: string): Either<any, string> {
    // Pyret string-tonumber: js-numbers fromString, nothing on failure.
    // The exact PyretNumber is converted to a JS number here because every
    // CLI consumer (inline-case-body-limit) wants a plain count.
    const n = jsnums.fromString(s, throwingErrbacks);
    if (n === false || n === null || n === undefined) {
      return right(`${paramName} expected a numeric argument, got ${toreprString(s)}`);
    }
    return left(jsnums.toFixnum(n));
  }
  parseString(): string { return '<number>'; }
}

export class ReadBool extends ParseParamBase {
  get $name(): 'read-bool' { return 'read-bool'; }
  parse(_argIndex: number, paramName: string, s: string): Either<any, string> {
    if (s === 'true') { return left(true); }
    else if (s === 'false') { return left(false); }
    else {
      return right(`${paramName} expected a boolean argument, got ${toreprString(s)}`);
    }
  }
  parseString(): string { return '(true|false)'; }
}

export class ReadString extends ParseParamBase {
  get $name(): 'read-string' { return 'read-string'; }
  parse(_argIndex: number, _paramName: string, s: string): Either<any, string> {
    return left(s);
  }
  parseString(): string { return '<string>'; }
}

export class ReadCustom extends ParseParamBase {
  get $name(): 'read-custom' { return 'read-custom'; }
  constructor(
    public name: string,
    public parser: (argIndex: number, paramName: string, s: string) => Either<any, string>
  ) { super(); }
  parse(argIndex: number, paramName: string, s: string): Either<any, string> {
    return this.parser(argIndex, paramName, s);
  }
  parseString(): string { return `<${this.name}>`; }
}

export type ParseParam = ReadNumber | ReadBool | ReadString | ReadCustom;
export function isParseParam(x: any): x is ParseParam { return x instanceof ParseParamBase; }

export const readNumber = new ReadNumber();
export const readBool = new ReadBool();
export const readString = new ReadString();
export function readCustom(
  name: string,
  parser: (argIndex: number, paramName: string, s: string) => Either<any, string>
): ReadCustom {
  return new ReadCustom(name, parser);
}

export const Num = readNumber;
export const Str = readString;
export const Bool = readBool;
export const Custom = readCustom;

// ---------- data ParsedArguments ----------

export class Success {
  get $name(): 'success' { return 'success'; }
  constructor(public parsed: Map<string, any>, public unknown: string[]) {}
}
export class ArgError {
  get $name(): 'arg-error' { return 'arg-error'; }
  constructor(public message: string, public partialResults: ParsedArguments) {}
}
export type ParsedArguments = Success | ArgError;
export function isParsedArguments(x: any): x is ParsedArguments {
  return x instanceof Success || x instanceof ArgError;
}
export function isSuccess(x: any): x is Success { return x instanceof Success; }
export function isArgError(x: any): x is ArgError { return x instanceof ArgError; }

// ---------- data ParamRepeat ----------
// _output strings are what format's ~a prints in usage/error text.

export class Once {
  get $name(): 'once' { return 'once'; }
  toString(): string { return 'may be used at most once'; }
}
export class Many {
  get $name(): 'many' { return 'many'; }
  toString(): string { return 'may be repeated'; }
}
export class RequiredOnce {
  get $name(): 'required-once' { return 'required-once'; }
  toString(): string { return 'must be used exactly once'; }
}
export class RequiredMany {
  get $name(): 'required-many' { return 'required-many'; }
  toString(): string { return 'must be used at least once'; }
}
export type ParamRepeat = Once | Many | RequiredOnce | RequiredMany;
export function isParamRepeat(x: any): x is ParamRepeat {
  return x instanceof Once || x instanceof Many ||
    x instanceof RequiredOnce || x instanceof RequiredMany;
}

export const once = new Once();
export const many = new Many();
export const requiredOnce = new RequiredOnce();
export const requiredMany = new RequiredMany();

// ---------- data Param ----------

export class Flag {
  get $name(): 'flag' { return 'flag'; }
  constructor(public repeated: ParamRepeat, public desc: string) {}
}
export class EqualsVal {
  get $name(): 'equals-val' { return 'equals-val'; }
  constructor(public parser: ParseParam, public repeated: ParamRepeat, public desc: string) {}
}
export class EqualsValDefault {
  get $name(): 'equals-val-default' { return 'equals-val-default'; }
  readonly default: any;
  constructor(
    public parser: ParseParam,
    defaultVal: any,
    public shortName: string | undefined,
    public repeated: ParamRepeat,
    public desc: string
  ) { this.default = defaultVal; }
}
export class NextVal {
  get $name(): 'next-val' { return 'next-val'; }
  constructor(public parser: ParseParam, public repeated: ParamRepeat, public desc: string) {}
}
export class NextValDefault {
  get $name(): 'next-val-default' { return 'next-val-default'; }
  readonly default: any;
  constructor(
    public parser: ParseParam,
    defaultVal: any,
    public shortName: string | undefined,
    public repeated: ParamRepeat,
    public desc: string
  ) { this.default = defaultVal; }
}
export type Param = Flag | EqualsVal | EqualsValDefault | NextVal | NextValDefault;

export function flag(repeated: ParamRepeat, desc: string): Flag {
  return new Flag(repeated, desc);
}
export function equalsVal(parser: ParseParam, repeated: ParamRepeat, desc: string): EqualsVal {
  return new EqualsVal(parser, repeated, desc);
}
export function equalsValDefault(
  parser: ParseParam, defaultVal: any, shortName: string | undefined,
  repeated: ParamRepeat, desc: string
): EqualsValDefault {
  return new EqualsValDefault(parser, defaultVal, shortName, repeated, desc);
}
export function nextVal(parser: ParseParam, repeated: ParamRepeat, desc: string): NextVal {
  return new NextVal(parser, repeated, desc);
}
export function nextValDefault(
  parser: ParseParam, defaultVal: any, shortName: string | undefined,
  repeated: ParamRepeat, desc: string
): NextValDefault {
  return new NextValDefault(parser, defaultVal, shortName, repeated, desc);
}

// fun is-Param_(l)
function isParam_(l: any): l is Param {
  return l instanceof Flag || l instanceof EqualsVal || l instanceof EqualsValDefault ||
    l instanceof NextVal || l instanceof NextValDefault;
}
export function isParam(x: any): x is Param { return isParam_(x); }

// ---------- usage-info ----------

// options : Map of Params (insertion order; see header comment on ordering)
export function usageInfo(options: Map<string, Param>): string[] {
  const optionInfo: string[] = [];
  for (const [key, param] of options) {
    switch (param.$name) {
      case 'flag':
        optionInfo.push(`  -${key}: ${param.desc} (${param.repeated})`);
        break;
      case 'equals-val':
        optionInfo.push(`  --${key}=${param.parser.parseString()}: ${param.desc} (${param.repeated})`);
        break;
      case 'equals-val-default': {
        const p = param as EqualsValDefault;
        if (p.shortName === undefined) {
          optionInfo.push(
            `  --${key}[=${p.parser.parseString()}]: ${p.desc} (${p.repeated}, default: ${toDisplay(p.default)})`);
        } else {
          optionInfo.push(
            `  --${key}[=${p.parser.parseString()}]: ${p.desc} (${p.repeated}, default: ${toDisplay(p.default)})\n` +
            `  -${p.shortName}: Defaults for ${p.desc} (${p.repeated})`);
        }
        break;
      }
      case 'next-val':
        optionInfo.push(`  --${key} ${param.parser.parseString()}: ${param.desc} (${param.repeated})`);
        break;
      case 'next-val-default': {
        const p = param as NextValDefault;
        if (p.shortName === undefined) {
          optionInfo.push(
            `  --${key} [${p.parser.parseString()}]: ${p.desc} (${p.repeated}, default: ${toDisplay(p.default)})`);
        } else {
          optionInfo.push(
            `  --${key} [${p.parser.parseString()}]: ${p.desc} (${p.repeated}, default: ${toDisplay(p.default)})\n` +
            `  -${p.shortName}: Defaults for ${p.desc} (${p.repeated})`);
        }
        break;
      }
      default:
        throw new Error(`Unknown Param variant: ${(param as any).$name}`);
    }
  }
  // The "[list: options]" below is literal text in the Pyret format string.
  return [`Usage: ${fileName} [list: options] where:`, ...optionInfo];
}

// ---------- parse-args ----------

// Pyret string-split: splits on the FIRST occurrence only.
function stringSplitOnce(s: string, sep: string): string[] {
  const idx = s.indexOf(sep);
  if (idx === -1) { return [s]; }
  return [s.substring(0, idx), s.substring(idx + sep.length)];
}

// options : Map of Params
// returns Map where names are same as names of options, values are parsed
// values (if present)
export function parseArgs(options: Map<string, Param>, args: string[]): ParsedArguments {
  const optsDict = options;

  // Collect short-name aliases (first conflict wins as an error, in
  // iteration order — Pyret iterates in string-dict hash order, we iterate
  // in insertion order; pyret.arr defines no short names so this cannot
  // diverge there).
  const optionAliases = new Map<string, string>();
  for (const [key, curOption] of optsDict) {
    if (curOption.$name === 'equals-val-default' || curOption.$name === 'next-val-default') {
      const short = (curOption as EqualsValDefault | NextValDefault).shortName;
      if (short !== undefined) {
        if (optsDict.has(short)) {
          return new ArgError(
            'Options map already includes entry for short-name ' + short,
            new Success(new Map(), []));
        }
        optionAliases.set(short, key);
      }
    }
  }
  const fullOptions = optsDict;

  function handleRepeated(
    results: ParsedArguments, repeated: ParamRepeat, name: string, val: any
  ): ParsedArguments {
    if (!isSuccess(results)) { return results; }
    const { parsed, unknown } = results;
    switch (repeated.$name) {
      case 'once':
      case 'required-once':
        if (parsed.has(name)) {
          return new ArgError(
            `Parsing command line options for ${fileName} failed: Option ${name} ${repeated}, and it has already been used`,
            results);
        } else {
          return new Success(mapSet(parsed, name, val), unknown);
        }
      case 'many':
      case 'required-many':
        if (parsed.has(name)) {
          return new Success(
            mapSet(parsed, name, [...mapGetValue(parsed, name), val]), unknown);
        } else {
          return new Success(mapSet(parsed, name, [val]), unknown);
        }
      default:
        throw new Error(`Unknown ParamRepeat variant: ${(repeated as any).$name}`);
    }
  }

  const required: string[] = [];
  for (const key of optsDict.keys()) {
    const repeated = mapGetValue(optsDict, key).repeated;
    if (repeated === requiredOnce || repeated === requiredMany) {
      required.push(key);
    }
  }

  function process(
    results: ParsedArguments, curIndex: number, remaining: string[]
  ): ParsedArguments {
    if (isArgError(results)) { return results; }
    const success = results as Success;
    if (remaining.length === 0) { return results; }
    const first = remaining[0];
    const moreArgs = remaining.slice(1);
    if (first.length < 2) {
      // STOP PROCESSING after first non-option value
      return new Success(success.parsed, [...success.unknown, ...remaining]);
    } else if (first.substring(0, 2) === '--') {
      const keyParts = stringSplitOnce(first.substring(2), '=');
      const key = keyParts[0];
      if (fullOptions.has(key)) {
        const param = mapGetValue(fullOptions, key);
        switch (param.$name) {
          case 'equals-val': {
            const { parser, repeated } = param as EqualsVal;
            if (keyParts.length === 1) {
              return new ArgError(
                `Option ${key} must be of the form --${key}=${parser.parseString()}`,
                results);
            } else {
              const val = keyParts[1];
              const parsedVal = parser.parse(curIndex, key, val);
              if (parsedVal.$name === 'left') {
                return process(handleRepeated(results, repeated, key, parsedVal.v), curIndex + 1, moreArgs);
              } else {
                return new ArgError(parsedVal.v, results);
              }
            }
          }
          case 'equals-val-default': {
            const p = param as EqualsValDefault;
            if (keyParts.length === 1) {
              return process(handleRepeated(results, p.repeated, key, p.default), curIndex + 1, moreArgs);
            } else {
              const val = keyParts[1];
              const parsedVal = p.parser.parse(curIndex, key, val);
              if (parsedVal.$name === 'left') {
                return process(handleRepeated(results, p.repeated, key, parsedVal.v), curIndex + 1, moreArgs);
              } else {
                return new ArgError(parsedVal.v, results);
              }
            }
          }
          case 'next-val': {
            const { parser, repeated } = param as NextVal;
            if (keyParts.length !== 1) {
              return new ArgError(
                `Command line option --${key} must be of the form --${key} ${parser.parseString()}, not --${key}=${parser.parseString()}`,
                results);
            } else if (moreArgs.length === 0) {
              return new ArgError(
                `Missing value for option ${key}; it must be of the form --${key} ${parser.parseString()}`,
                results);
            } else {
              const val = moreArgs[0];
              const rest = moreArgs.slice(1);
              if (val.charAt(0) === '-') {
                const parsedVal = parser.parse(curIndex, key, val);
                if (parsedVal.$name === 'left') {
                  return process(handleRepeated(results, repeated, key, parsedVal.v), curIndex + 2, rest);
                } else {
                  return new ArgError(
                    `Missing value for option ${key}; it must be of the form --${key} ${parser.parseString()}`,
                    results);
                }
              } else {
                const parsedVal = parser.parse(curIndex + 1, key, val);
                if (parsedVal.$name === 'left') {
                  return process(handleRepeated(results, repeated, key, parsedVal.v), curIndex + 2, rest);
                } else {
                  return new ArgError(parsedVal.v, results);
                }
              }
            }
          }
          case 'next-val-default': {
            const p = param as NextValDefault;
            if (keyParts.length !== 1) {
              return new ArgError(
                `Command line option --${key} must be of the form --${key} ${p.parser.parseString()}, not --${key}=${p.parser.parseString()}`,
                results);
            } else if (moreArgs.length === 0) {
              return handleRepeated(results, p.repeated, key, p.default);
            } else {
              const val = moreArgs[0];
              const rest = moreArgs.slice(1);
              if (val.charAt(0) === '-') {
                const parsedVal = p.parser.parse(curIndex, key, val);
                if (parsedVal.$name === 'left') {
                  return process(handleRepeated(results, p.repeated, key, parsedVal.v), curIndex + 2, rest);
                } else {
                  return process(handleRepeated(results, p.repeated, key, p.default), curIndex + 1, moreArgs);
                }
              } else {
                const parsedVal = p.parser.parse(curIndex, key, val);
                if (parsedVal.$name === 'left') {
                  // NOTE: cur-index + 1 (not + 2) in the original
                  return process(handleRepeated(results, p.repeated, key, parsedVal.v), curIndex + 1, rest);
                } else {
                  return new ArgError(parsedVal.v, results);
                }
              }
            }
          }
          case 'flag':
            return new ArgError(
              `Command line option -${key} does not start with two dashes`, results);
          default:
            throw new Error(`Unknown Param variant: ${(param as any).$name}`);
        }
      } else {
        return new ArgError('Unknown command line option --' + key, results);
      }
    } else if (first.substring(0, 1) === '-') {
      const key = first.substring(1);
      let lookup: Param | undefined;
      if (optionAliases.has(key) && fullOptions.has(mapGetValue(optionAliases, key))) {
        lookup = mapGetValue(fullOptions, mapGetValue(optionAliases, key));
      } else if (fullOptions.has(key)) {
        lookup = mapGetValue(fullOptions, key);
      } else {
        lookup = undefined;
      }
      if (isParam_(lookup)) {
        switch (lookup.$name) {
          case 'flag':
            return process(handleRepeated(results, lookup.repeated, key, true), curIndex + 1, moreArgs);
          case 'equals-val-default':
            // get-value raises if `key` is not actually an alias, exactly
            // like the Pyret original.
            return process(
              handleRepeated(results, lookup.repeated, mapGetValue(optionAliases, key), (lookup as EqualsValDefault).default),
              curIndex + 1, moreArgs);
          case 'next-val-default':
            return process(
              handleRepeated(results, lookup.repeated, mapGetValue(optionAliases, key), (lookup as NextValDefault).default),
              curIndex + 1, moreArgs);
          default:
            return new ArgError(
              `Command line option --${key} must start with two dashes`, results);
        }
      } else {
        return new ArgError('Unknown command line option -' + key, results);
      }
    } else {
      // STOP PROCESSING after first non-option value
      return new Success(success.parsed, [...success.unknown, ...remaining]);
    }
  }

  const parsedResults = process(new Success(new Map(), []), 1, args);
  if (isSuccess(parsedResults)) {
    const { parsed, unknown: other } = parsedResults;
    let filledMissingDefaults = parsed;
    for (const [key, param] of optsDict) {
      if (param.$name === 'next-val-default' || param.$name === 'equals-val-default') {
        const p = param as NextValDefault | EqualsValDefault;
        if (!filledMissingDefaults.has(key) && (p.repeated === once || p.repeated === many)) {
          filledMissingDefaults = mapSet(filledMissingDefaults, key, p.default);
        }
      }
    }
    const missingArgs = required.filter((key) => !filledMissingDefaults.has(key));
    if (missingArgs.length === 0) {
      return new Success(filledMissingDefaults, other);
    } else {
      return new ArgError(
        `Command line option validation for ${fileName} failed: The following options are required but not found: ${stringListToRepr(missingArgs)}`,
        parsedResults);
    }
  } else {
    return parsedResults;
  }
}

// Parses the actual command line arguments against the provided options map
export function parseCmdline(options: Map<string, Param>): ParsedArguments {
  return parseArgs(options, otherArgs);
}
