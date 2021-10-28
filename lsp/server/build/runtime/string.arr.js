var EQUALITY = require("./equality.js");
var NUMBER = require("./js-numbers.js");
var OPTION = require("./option.arr.js");
var LISTS = require("./lists.arr.js");
function stringToNumber(s) {
    var result = NUMBER['fromString'](s);
    if (result === false) {
        return OPTION['none'];
    }
    else {
        return OPTION['some'](result);
    }
}
module.exports = {
    'string-equal': function (s1, s2) {
        return EQUALITY.equalAlways(s1, s2);
    },
    'string-contains': function (toSearchIn, toSearchFor) {
        return toSearchIn.includes(toSearchFor);
    },
    'string-append': function (front, back) {
        return front.concat(back);
    },
    'string-length': function (str) {
        return str.length;
    },
    'string-to-number': stringToNumber,
    // n is a PyretNumber
    'string-repeat': function (str, n) {
        return str.repeat(NUMBER["toFixnum"](n));
    },
    'string-substring': function (str, start, exEnd) {
        start = NUMBER["toFixnum"](start);
        exEnd = NUMBER["toFixnum"](exEnd);
        // TODO(alex): Better messages
        if (start < 0) {
            throw "Invalid inclusive start index: " + start;
        }
        if (exEnd > str.length || exEnd < 0) {
            throw "Invalid exclusive end index: " + exEnd;
        }
        if (start > exEnd) {
            throw "Invalid inclusive start index vs end index: " + start + " > " + exEnd;
        }
        return str.substring(start, exEnd);
    },
    'string-index-of': function (original, toFind) {
        return original.indexOf(toFind);
    },
    'string-replace': function (original, toFind, replacement) {
        if (toFind.length === 0) {
            var result = "";
            for (var i = 0; i < original.length; i++) {
                result = result.concat(original[i]);
                if (i < original.length - 1) {
                    result = result.concat(replacement);
                }
            }
            return result;
        }
        else {
            // From SO:
            // https://stackoverflow.com/questions/1144783/how-to-replace-all-occurrences-of-a-string/
            return original.replace(new RegExp(toFind, 'g'), replacement);
            // TODO(alex): replaceAll() not yet supported everywhere
            // return original.replaceAll(toFind, replacement);
        }
    },
    // Returns a PyretList<String>
    'string-split': function (original, splitOn) {
        if (EQUALITY.equalAlways(splitOn, "")) {
            return LISTS["list"].make(["", original]);
        }
        else {
            var separated = original.split(splitOn);
            if (separated.length > 1) {
                var first = separated[0];
                var rest = separated.slice(1).join(splitOn);
                return LISTS["raw-array-to-list"]([first, rest]);
            }
            else {
                return LISTS["raw-array-to-list"](separated);
            }
        }
    },
    // Returns a PyretList<String>
    'string-split-all': function (original, splitOn) {
        var separated = original.split(splitOn);
        return LISTS["raw-array-to-list"](separated);
    },
    // Returns a PyretList<String>
    'string-explode': function (original) {
        var separated = original.split("");
        return LISTS["raw-array-to-list"](separated);
    },
    // n is a PyretNumber
    'string-char-at': function (original, n) {
        return original.charAt(NUMBER["toFixnum"](n));
    },
    'string-to-upper': function (str) {
        return str.toUpperCase();
    },
    'string-toupper': function (str) {
        return str.toUpperCase();
    },
    'string-tolower': function (str) {
        return str.toLowerCase();
    },
    'string-to-lower': function (str) {
        return str.toLowerCase();
    },
    'string-to-code-point': function (str) {
        if (str.length !== 1) {
            throw "String length !== 1";
        }
        return str.codePointAt(0);
    },
    'string-to-code-points': function (str) {
        var results = new Array();
        for (var i = 0; i < str.length; i++) {
            console.log("FOO " + i + "::" + str.codePointAt(i) + " (" + str + ")");
            results.push(str.charCodeAt(i));
        }
        return LISTS["list"].make(results);
    },
    // point is a PyretNumber
    'string-from-code-point': function (point) {
        point = NUMBER["toFixnum"](NUMBER["floor"](point));
        return String.fromCodePoint(point);
    },
    // points is a PyretList<PyretNumber>
    'string-from-code-points': function (points) {
        var rawArrayPoints = LISTS["to-raw-array"](points);
        var stringArray = rawArrayPoints.map(function (pyNum) {
            var fixedNum = NUMBER["toFixnum"](NUMBER["floor"](pyNum));
            return String.fromCodePoint(fixedNum);
        });
        return stringArray.join("");
    }
};
