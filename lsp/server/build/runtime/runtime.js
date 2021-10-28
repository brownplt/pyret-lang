"use strict";
exports.__esModule = true;
exports.$imgUrlProxy = exports.$setImgUrlProxyWrapper = exports.run = exports.safeVoidCallback = exports.pauseStack = exports.traceValue = exports.$setCheckBlockExecutor = exports.$setCheckBlockFilter = exports.$setSpyValueHandler = exports.$setSpyMessageHandler = void 0;
// TODO(alex): `import type` syntax is causing a parsing error
// import type { NumericErrorCallbacks } from "equality";
/*
 * 'export named-js-value' desugars into 'exports.name = js-value'
 *
 * https://stackoverflow.com/questions/16383795/difference-between-module-exports-and-exports-in-the-commonjs-module-system
 *
 */
var _NUMBER = require("./js-numbers.js");
var _EQUALITY = require('./equality.js');
var _PRIMITIVES = require("./primitives.js");
// *********Spy Stuff*********
var $spyMessageHandler = function (data) {
    if (data.message) {
        console.log("Spying \"" + data.message + "\" (at " + data.loc + ")");
    }
    else {
        console.log("Spying (at " + data.loc + ")");
    }
};
var $spyValueHandler = function (data) {
    console.log("    " + data.key + ": " + data.value + " (at " + data.loc + ")");
};
function $setSpyMessageHandler(handler) {
    $spyMessageHandler = handler;
}
exports.$setSpyMessageHandler = $setSpyMessageHandler;
function $setSpyValueHandler(handler) {
    $spyValueHandler = handler;
}
exports.$setSpyValueHandler = $setSpyValueHandler;
function _not(x) { return !x; }
function _spy(spyObject) {
    var message = spyObject.message();
    var spyLoc = spyObject.loc;
    if ($spyMessageHandler) {
        $spyMessageHandler({ message: message, loc: spyLoc });
    }
    var exprs = spyObject.exprs;
    for (var i = 0; i < exprs.length; i++) {
        var key = exprs[i].key;
        var loc = exprs[i].loc;
        var value = exprs[i].expr();
        if ($spyValueHandler) {
            $spyValueHandler({ key: key, value: value, loc: loc });
        }
    }
}
// *********Check Stuff*********
var _globalCheckContext = [];
var _globalCheckResults = {};
// TODO: Pass in the URI to the check test executors
//   so we can attempt to filter check blocks by module
// TODO: Add check test override
// TODO: Need to expose an check runner test API to the IDE
var $checkBlockExecutor = eagerCheckBlockRunner;
var $checkBlockFilter = null;
function $setCheckBlockFilter(filter) {
    $checkBlockFilter = filter;
}
exports.$setCheckBlockFilter = $setCheckBlockFilter;
function $setCheckBlockExecutor(executor) {
    $checkBlockExecutor = executor;
}
exports.$setCheckBlockExecutor = $setCheckBlockExecutor;
function checkBlockHandler(srcloc, name, checkBlock) {
    $checkBlockExecutor(srcloc, name, checkBlock);
}
function getCheckResults(uri) {
    return _globalCheckResults[uri].slice();
}
function clearChecks(uri) { _globalCheckResults[uri] = []; }
function checkResults(uri) {
    var errorCount = 0;
    _globalCheckResults[uri].forEach(function (result) {
        if (!result.success) {
            errorCount += 1;
        }
    });
    if (errorCount === 0) {
        console.log("Looks shipshape, all tests passed, mate!");
    }
    else {
        console.log("Some tests failed.");
    }
    _globalCheckResults[uri].forEach(function (result) {
        var result_lhs = JSON.stringify(result.lhs, null, "\t");
        var result_rhs = JSON.stringify(result.rhs, null, "\t");
        if (result.success) {
            console.log("[PASS] ([" + result.path + "], at " + result.loc + ")");
        }
        else {
            if (result.exception) {
                console.log("[FAIL] Caught exception <" + result.exception.toString() + ">. Found <" + result_lhs + ">. Expected <" + result_rhs + "> ([" + result.path + "], at " + result.loc + ")");
            }
            else {
                console.log("[FAIL] Found <" + result_lhs + ">. Expected <" + result_rhs + "> ([" + result.path + "], at " + result.loc + ")");
            }
        }
    });
    return getCheckResults(uri);
}
function eagerCheckTest(lhs, rhs, test, loc) {
    var uri = getUriForCheckLoc(loc);
    if (!(uri in _globalCheckResults)) {
        _globalCheckResults[uri] = [];
    }
    var lhs_expr_eval = {
        value: undefined,
        exception: false,
        exception_val: undefined
    };
    var rhs_expr_eval = {
        value: undefined,
        exception: false,
        exception_val: undefined
    };
    try {
        lhs_expr_eval.value = lhs();
    }
    catch (e) {
        lhs_expr_eval.exception = true;
        lhs_expr_eval.exception_val = e;
    }
    try {
        rhs_expr_eval.value = rhs();
    }
    catch (e) {
        rhs_expr_eval.exception = true;
        rhs_expr_eval.exception_val = e;
    }
    try {
        var result = test(lhs_expr_eval, rhs_expr_eval);
        _globalCheckResults[uri].push({
            success: result.success,
            path: _globalCheckContext.join(),
            loc: loc,
            lhs: result.lhs,
            rhs: result.rhs,
            exception: undefined
        });
    }
    catch (e) {
        _globalCheckResults[uri].push({
            success: false,
            path: _globalCheckContext.join(),
            loc: loc,
            lhs: lhs_expr_eval,
            rhs: rhs_expr_eval,
            exception: e
        });
    }
}
// TODO(alex): Common URI object that's not a string
function eagerCheckBlockRunner(uri, name, checkBlock) {
    if ($checkBlockFilter && !$checkBlockFilter(uri, name)) {
        return;
    }
    _globalCheckContext.push(name);
    try {
        checkBlock();
    }
    catch (e) {
        throw e;
    }
    finally {
        _globalCheckContext.pop();
    }
}
var _globalTraceValues = {};
function getUri(loc) {
    return loc[0];
}
function getUriForCheckLoc(loc) {
    // NOTE(joe/luna): The locations look like file:///path:start-end. This gets
    // the bit between the two colons (one after file:, one after path:)
    return loc.substring(0, loc.indexOf(":", loc.indexOf(":") + 1));
}
// ********* Other Functions *********
function traceValue(loc, value) {
    // NOTE(alex): stubbed out until we decide what to actually do with it
    var uri = getUri(loc);
    if (!(uri in _globalTraceValues)) {
        _globalTraceValues[uri] = [];
    }
    _globalTraceValues[uri].push({ srcloc: loc, value: value });
    return value;
}
exports.traceValue = traceValue;
function getTraces(uri) { return _globalTraceValues[uri]; }
function clearTraces(uri) { _globalTraceValues[uri] = []; }
// Allow '+' for string concat.
// Otherwise, defer to the number library.
function customPlus(lhs, rhs, errbacks) {
    if ((typeof lhs === "object") && ("_plus" in lhs)) {
        return lhs._plus(rhs);
    }
    else if (_PRIMITIVES.isString(lhs) && _PRIMITIVES.isString(rhs)) {
        return lhs + rhs;
    }
    else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.add(lhs, rhs, errbacks);
    }
    else {
        // NOTE: may be a dynamic error
        try {
            return lhs + rhs;
        }
        catch (error) {
            throw new Error("Unable to perform '+' on (" + lhs + ") and (" + rhs + ")");
        }
    }
}
function customMinus(lhs, rhs, errbacks) {
    if ((typeof lhs === "object") && ("_minus" in lhs)) {
        return lhs._minus(rhs);
    }
    else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.subtract(lhs, rhs, errbacks);
    }
    else {
        // NOTE: may be a dynamic error
        try {
            return lhs - rhs;
        }
        catch (error) {
            throw new Error("Unable to perform '-' on (" + lhs + ") and (" + rhs + ")");
        }
    }
}
function customTimes(lhs, rhs, errbacks) {
    if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.multiply(lhs, rhs, errbacks);
    }
    else if ((typeof lhs === "object") && ("_times" in lhs)) {
        return lhs._times(rhs);
    }
    else {
        // NOTE: may be a dynamic error
        try {
            return lhs * rhs;
        }
        catch (error) {
            throw new Error("Unable to perform '*' on (" + lhs + ") and (" + rhs + ")");
        }
    }
}
function customDivide(lhs, rhs, errbacks) {
    if ((typeof lhs === "object") && ("_divide" in lhs)) {
        return lhs._divide(rhs);
    }
    else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.divide(lhs, rhs, errbacks);
    }
    else {
        // NOTE: may be a dynamic error
        try {
            return lhs / rhs;
        }
        catch (error) {
            throw new Error("Unable to perform '/' on (" + lhs + ") and (" + rhs + ")");
        }
    }
}
/* @stopify flat */
function pauseStack(callback) {
    // @ts-ignore
    return $STOPIFY.pauseK(/* @stopify flat */ function (kontinue) {
        return callback({
            resume: /* @stopify flat */ function (val) { return kontinue({ type: "normal", value: val }); },
            error: /* @stopify flat */ function (err) { return kontinue({ type: "error", error: err }); }
        });
    });
}
exports.pauseStack = pauseStack;
function safeVoidCallback(f) {
    return function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        return run(function () { return f.apply(void 0, args); }, function (a) {
            if (a.type === 'error') {
                console.error("A safeVoidCallback failed with an error: ", a);
            }
            else {
                console.log("A safeVoidCallback succeeded");
            }
        });
    };
}
exports.safeVoidCallback = safeVoidCallback;
function run(f, onDone) {
    // @ts-ignore
    return $STOPIFY.runStopifiedCode(f, onDone);
}
exports.run = run;
var allModules = {};
function addModule(uri, vals) {
    allModules[uri] = { values: vals };
}
function getModuleValue(uri, k) {
    return allModules[uri].values[k];
}
function installMethod(obj, name, method) {
    Object.defineProperty(obj, name, { value: method, writable: false });
    return method;
}
function setupMethodGetters(obj) {
    var extension = {};
    for (var k in obj.$methods) {
        extension[k] = { get: obj.$methods[k], configurable: true };
    }
    Object.defineProperties(obj, extension);
    return obj;
}
// TODO(alex): common Pyret error objects
function raise(msg) {
    // NOTE(alex): Changing the representation needs to be reflected in raiseExtract()
    throw msg;
}
function raiseExtract(exception) {
    // NOTE(alex): Used by `raises` check operator
    //   Any changes to the `raise` exception format needs to be reflected
    //   here as well.
    return exception.toString();
}
// NOTE(alex): stub implementation used by testing infrastructure
function torepr(v) {
    return JSON.stringify(v);
}
function customThrow(exn) {
    exn.toString = function () { return JSON.stringify(this); };
    throw new Error(exn);
}
var imageUrlProxyWrapper = function (url) {
    return url;
};
function $setImgUrlProxyWrapper(wrapperFn) {
    imageUrlProxyWrapper = wrapperFn;
}
exports.$setImgUrlProxyWrapper = $setImgUrlProxyWrapper;
function $imgUrlProxy(url) {
    return imageUrlProxyWrapper(url);
}
exports.$imgUrlProxy = $imgUrlProxy;
module.exports["addModule"] = addModule;
module.exports["getModuleValue"] = getModuleValue;
// Hack needed b/c of interactions with the 'export' keyword
// Pyret instantiates singleton data varaints by taking a reference to the value
// TODO(alex): Should Pyret perform a function call to create a singleton data variant
module.exports["Equal"] = _EQUALITY.Equal;
module.exports["NotEqual"] = _EQUALITY.NotEqual;
module.exports["Uknown"] = _EQUALITY.Unknown;
// Hack needed to match generate Pyret-code
module.exports["is-Equal"] = _EQUALITY.isEqual;
module.exports["is-NotEqual"] = _EQUALITY.isNotEqual;
module.exports["is-Unknown"] = _EQUALITY.isUnknown;
module.exports["equal-now"] = _EQUALITY.equalNow;
module.exports["equal-now3"] = _EQUALITY.equalNow3;
module.exports["equal-always"] = _EQUALITY.equalAlways;
module.exports["equal-always3"] = _EQUALITY.equalAlways3;
module.exports["identical"] = _EQUALITY.identical;
module.exports["identical3"] = _EQUALITY.identical3;
// Expected runtime functions
module.exports["raise"] = raise;
module.exports["$raiseExtract"] = raiseExtract;
module.exports["trace-value"] = traceValue;
module.exports["$getTraces"] = getTraces;
module.exports["$clearTraces"] = clearTraces;
module.exports["$spy"] = _spy;
module.exports["$extend"] = _PRIMITIVES.extend;
module.exports["$installMethod"] = installMethod;
module.exports["$setupMethodGetters"] = setupMethodGetters;
module.exports["$makeDataValue"] = _PRIMITIVES.makeDataValue;
module.exports["$createVariant"] = _PRIMITIVES.createVariant;
module.exports["$checkTest"] = eagerCheckTest;
module.exports["$checkBlock"] = checkBlockHandler;
module.exports["$checkResults"] = checkResults;
module.exports["$getCheckResults"] = getCheckResults;
module.exports["$clearChecks"] = clearChecks;
module.exports["$makeRational"] = _NUMBER["makeRational"];
module.exports["$makeRoughnum"] = _NUMBER["makeRoughnum"];
module.exports["$errCallbacks"] = _EQUALITY.NumberErrbacks;
module.exports["_not"] = _not;
module.exports["_plus"] = customPlus;
module.exports["_minus"] = customMinus;
module.exports["_times"] = customTimes;
module.exports["_divide"] = customDivide;
module.exports["_lessthan"] = _EQUALITY._lessthan;
module.exports["_greaterthan"] = _EQUALITY._greaterthan;
module.exports["_lessequal"] = _EQUALITY._lessequal;
module.exports["_greaterequal"] = _EQUALITY._greaterequal;
module.exports["_makeNumberFromString"] = _NUMBER['fromString'];
module.exports["PTuple"] = _PRIMITIVES["PTuple"];
module.exports["$makeMethodBinder"] = _PRIMITIVES["makeMethodBinder"];
module.exports["$torepr"] = torepr;
module.exports["$nothing"] = _PRIMITIVES["$nothing"];
module.exports["$customThrow"] = customThrow;
module.exports["$messageThrow"] = function (srcloc, message) {
    customThrow({
        "message": message,
        "$srcloc": srcloc
    });
};
module.exports["throwUnfinishedTemplate"] = function (srcloc) {
    customThrow({
        "$template-not-finished": srcloc
    });
};
// TODO(alex): Fill out exceptions with useful info
module.exports["throwNoCasesMatched"] = function (srcloc) {
    customThrow({
        "kind": "throwNoCasesMatched",
        "$srcloc": srcloc
    });
};
module.exports["throwNoBranchesMatched"] = function (srcloc) {
    customThrow({
        "kind": "throwNoBranchesMatched",
        "$srcloc": srcloc
    });
};
// TODO(alex): is exn necessary?
module.exports["throwNonBooleanOp"] = function (srcloc) {
    customThrow({
        "kind": "throwNonBooleanOp",
        "$srcloc": srcloc
    });
};
// TODO(alex): is exn necessary?
module.exports["throwNonBooleanCondition"] = function (srcloc) {
    customThrow({
        "kind": "throwNonBooleanCondition",
        "$srcloc": srcloc
    });
};
// NOTE: "is-roughly" => "is%(within(0.000001))"
module.exports["within"] = _EQUALITY["within"];
