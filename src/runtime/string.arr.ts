const EQUALITY = require("./equality.js");
const NUMBER = require("./js-numbers.js");
const OPTION = require("./option.arr.js");
const RUNTIME = require("./runtime.js");

function stringToNumber(s: string): any {
    var result = NUMBER['fromString'](s);
    if (result === false) {
        return OPTION['none'];
    } else {
        return OPTION['some'](result);
    }
}

function stringToNum(s: string): any {
    var result = NUMBER['fromString'](s);
    if (result === false) {
        RUNTIME.throwError("message-exception",
            `string-to-num expected a numeric string, got ${RUNTIME.toRepr(s)}`);
    } else {
        return result;
    }
}

const ASTRAL_CUTOFF = 65535;
function check_astral(n : number) {
    if(n > ASTRAL_CUTOFF) {
        throw new Error(`Invalid code point: ${n}. Code points must be smaller than ${ASTRAL_CUTOFF}`);
    }
}

module.exports = {
    'string-equal': function(s1: string, s2: string): boolean {
        return EQUALITY.equalAlways(s1, s2);
    },

    'string-contains': function(toSearchIn: string, toSearchFor: string): boolean {
        return toSearchIn.includes(toSearchFor);
    },

    'string-append': function(front: string, back: string): string {
        return front.concat(back);
    },

    'string-length': function(str: string): number {
        return str.length;
    },

    'string-to-number': stringToNumber,
    'string-to-num': stringToNum,
    'string-tonumber': stringToNum,

    'string-is-number': function(s) {
      var num = NUMBER['fromString'](s);
      if(num !== false) { return true; }
      else { return false; }
    },

    // n is a PyretNumber
    'string-repeat': function(str: string, n): string {
        return str.repeat(NUMBER["toFixnum"](n));
    },

    'string-substring': function(str: string, start, exEnd): string {
        if(!NUMBER["isInteger"](start) || !NUMBER["isInteger"](exEnd)) {
            throw new Error(`Expected NumInteger, for string-substring, but got ${start} and ${exEnd}`);
        }
        start = NUMBER["toFixnum"](start);
        exEnd = NUMBER["toFixnum"](exEnd);


        // TODO(alex): Better messages
        if (start < 0) {
            throw `Invalid inclusive start index: ${start}`;
        }

        if (exEnd > str.length || exEnd < 0) {
            throw `Invalid exclusive end index: ${exEnd}`;
        }

        if (start > exEnd) {
            throw `Invalid inclusive start index vs end index: ${start} > ${exEnd}`;
        }
        return str.substring(start, exEnd);
    },

    'string-index-of': function(original: string, toFind: string): number {
        return original.indexOf(toFind);
    },

    'string-replace': function(original: string, toFind: string, replacement: string): string {

        if (toFind.length === 0) {
            let result = "";
            for (let i = 0; i < original.length; i++) {
                result = result.concat(original[i]);
                if (i < original.length - 1) {
                    result = result.concat(replacement);
                }
            }
            return result;
        } else {
            // From SO:
            // https://stackoverflow.com/questions/1144783/how-to-replace-all-occurrences-of-a-string/
            return original.replace(new RegExp(toFind, 'g'), replacement);
            // TODO(alex): replaceAll() not yet supported everywhere
            // return original.replaceAll(toFind, replacement);
        }
    },

    // Returns a PyretList<String>
    'string-split': function(original: string, splitOn: string) {
        const LISTS = require("./" + "lists.arr.js");
        if (EQUALITY.equalAlways(splitOn, "")) {
            return LISTS["list"].make(["", original]);
        } else {
            const separated = original.split(splitOn);
            if (separated.length > 1) {
                const first = separated[0];
                const rest = separated.slice(1).join(splitOn);
                return LISTS["raw-array-to-list"]([first, rest]);
            } else {
                return LISTS["raw-array-to-list"](separated);
            }
        }
    },

    // Returns a PyretList<String>
    'string-split-all': function(original: string, splitOn: string) {
        const separated = original.split(splitOn);
        const LISTS = require("./" + "lists.arr.js");
        return LISTS["raw-array-to-list"](separated);
    },

    // Returns a PyretList<String>
    'string-explode': function(original: string) {
        const separated = original.split("");
        const LISTS = require("./" + "lists.arr.js");
        return LISTS["raw-array-to-list"](separated);
    },

    // n is a PyretNumber
    'string-char-at': function(original: string, n): string {
        if(n < 0 || !NUMBER['isInteger'](n)) {
            throw new Error(`string-char-at: expected a non-negative integer for the index, but got ${n}`);
        }
        if(n >= original.length) {
            throw new Error(`string-char-at: index ${n} is greater than the largest index in the string ${original}`);
        }
        return original.charAt(NUMBER["toFixnum"](n));
    },

    'string-to-upper': function(str: string): string {
        return str.toUpperCase();
    },

    'string-toupper': function(str: string): string {
        return str.toUpperCase();
    },

    'string-tolower': function(str: string): string {
        return str.toLowerCase();
    },

    'string-to-lower': function(str: string): string {
        return str.toLowerCase();
    },

    'string-to-code-point': function(str: string): number {
        if (str.length !== 1) {
            throw new Error(`string-to-code-point expects a string of length exactly one, got ${str}`);
        }

        const ans = str.codePointAt(0);
        if (ans === undefined) {
            throw new Error(`string-to-code-point could not extract codepoint of ${str}`);
        }
        return ans;
    },

    'string-to-code-points': function(str: string): number {
        const results = new Array();
        for (let i = 0; i < str.length; i++) {
            console.log(`FOO ${i}::${str.codePointAt(i)} (${str})`);
            results.push(str.charCodeAt(i));
        }

        const LISTS = require("./" + "lists.arr.js");
        return LISTS["list"].make(results);
    },

    // point is a PyretNumber
    'string-from-code-point': function(point): string {
        RUNTIME.checkNumNatural(point);
        point = NUMBER["toFixnum"](NUMBER["floor"](point));
        check_astral(point);
        return String.fromCodePoint(point);
    },

    // points is a PyretList<PyretNumber>
    'string-from-code-points': function(points): string {
        const LISTS = require("./" + "lists.arr.js");
        const rawArrayPoints = LISTS["to-raw-array"](points);
        const stringArray = rawArrayPoints.map((pyNum) => {
            RUNTIME.checkNumNatural(pyNum);
            const fixedNum = NUMBER["toFixnum"](NUMBER["floor"](pyNum));
            check_astral(fixedNum);
            return String.fromCodePoint(fixedNum);
        });

        return stringArray.join("");
    },
    'string-starts-with': function(s: string, fragment: string) {
        return s.startsWith(fragment);
    },
    'string-ends-with': function(s: string, fragment: string) {
        return s.endsWith(fragment);
    },
    'is-string': function(x : any) { return typeof x === 'string'; }
};
