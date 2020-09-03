const EQUALITY = require("./equality.js");
const NUMBER = require("./js-numbers.js");
const OPTION = require("./option.arr.js");
const LISTS = require("./lists.arr.js");

function stringToNumber(s: string): any {
    var result = NUMBER['fromString'](s);
    if (result === false) {
        return OPTION['none'];
    } else {
        return OPTION['some'](result);
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

    // n is a PyretNumber
    'string-repeat': function(str: string, n): string {
        return str.repeat(NUMBER["toFixnum"](n));
    },

    'string-substring': function(str: string, start, exEnd): string {
        start = NUMBER["toFixnum"](start);
        exEnd = NUMBER["toFixnum"](exEnd);
        return str.substring(start, exEnd);
    },

    'string-index-of': function(original: string, toFind: string): number {
        return original.indexOf(toFind);
    },

    'string-replace': function(original: string, toFind: string, replacement: string): string {

        if (toFind.length === 0) {
            let result = "";
            for (let i = 0; i < original.length; i++) {
                result.concat(original[i]);
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
        if (EQUALITY.equalAlways(splitOn, "")) {
            return LISTS["list"].make(["", original]);
        } else {
            const separated = original.split(splitOn);
            const first = separated[0];
            const rest = separated.slice(1).join();
            return LISTS["raw-array-to-list"]([first, rest]);
        }
    },

    // Returns a PyretList<String>
    'string-split-all': function(original: string, splitOn: string) {
        const separated = original.split(splitOn);
        return LISTS["raw-array-to-list"](separated);
    },

    // Returns a PyretList<String>
    'string-explode': function(original: string) {
        const separated = original.split("");
        return LISTS["raw-array-to-list"](separated);
    },

    // n is a PyretNumber
    'string-char-at': function(original: string, n): string {
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
            throw "String length !== 1";
        }

        return str.codePointAt(1);
    },

    'string-to-code-points': function(str: string): number {
        const results = new Array();
        for (let i = 0; i < str.length; i++) {
            results.push(str.codePointAt(i));
        }

        return LISTS["list"].make(results);
    },

    // point is a PyretNumber
    'string-from-code-point': function(point): string {
        point = NUMBER["toFixnum"](NUMBER["floor"](point));
        return String.fromCodePoint(point);
    },

    // points is a PyretList<PyretNumber>
    'string-from-code-points': function(points): string {
        const rawArrayPoints = LISTS["to-raw-array"](points);
        const stringArray = rawArrayPoints.map((pyNum) => {
            const fixedNum = NUMBER["toFixnum"](NUMBER["floor"](pyNum));
            return String.fromCodePoint(fixedNum);
        });

        return stringArray.join();
    }
};
