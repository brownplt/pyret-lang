"use strict";
exports.__esModule = true;
exports.applyBrand = exports.hasBrand = exports.makeMethodBinder = exports.isPRef = exports.isArray = exports.isPTuple = exports.isRawObject = exports.isDataVariant = exports.isString = exports.isBoolean = exports.isRoughNumber = exports.isNumber = exports.isNothing = exports.isMethod = exports.isFunction = exports.isTable = exports.isRow = exports.makeDataValue = exports.createVariant = exports.extend = exports.PTuple = exports.$nothing = exports.$PMethodBrand = exports.$PRefBrand = exports.$PTupleBrand = exports.$PTableBrand = exports.$PRowBrand = void 0;
var _NUMBER = require("./js-numbers.js");
var $PMethodBrand = "METHOD";
exports.$PMethodBrand = $PMethodBrand;
var $PRowBrand = "row";
exports.$PRowBrand = $PRowBrand;
var $PTableBrand = "$table";
exports.$PTableBrand = $PTableBrand;
var $PTupleBrand = "tuple";
exports.$PTupleBrand = $PTupleBrand;
var $PRefBrand = "ref";
exports.$PRefBrand = $PRefBrand;
var $nothing = undefined;
exports.$nothing = $nothing;
// ********* Runtime Type Representations (Non-Primitives) *********
function PTuple(values) {
    values["$brand"] = $PTupleBrand;
    return values;
}
exports.PTuple = PTuple;
function extend(obj, extension) {
    for (var k in obj.$methods) {
        if (!(extension.hasOwnProperty(k))) {
            Object.defineProperty(extension, k, { configurable: true, get: obj.$methods[k] });
        }
    }
    Object.setPrototypeOf(extension, obj);
    Object.setPrototypeOf(extension.$methods, obj.$methods);
    return extension;
}
exports.extend = extend;
var variantCounter = 1;
var variants = [];
function createVariant(sharedBase, extension, meta) {
    var extended = extend(sharedBase, extension);
    var metaExtended = Object.assign(extended, meta);
    // NOTE(joe): we cannot pass extended as an argument to this function, because
    // sharedBased/extension/meta can't easily have a cycle between them due to
    // codegen passing them in as object literals.
    metaExtended.$variant = variantCounter++;
    variants.push(metaExtended);
    return metaExtended;
}
exports.createVariant = createVariant;
function makeDataValue(obj, extension) {
    Object.setPrototypeOf(extension, obj);
    extension.$methods = {};
    return extension;
}
exports.makeDataValue = makeDataValue;
function isRow(val) {
    return hasBrand($PRowBrand, val);
}
exports.isRow = isRow;
function isTable(val) {
    return hasBrand($PTableBrand, val);
}
exports.isTable = isTable;
function isFunction(obj) {
    return (typeof obj === "function") && !(isMethod(obj));
}
exports.isFunction = isFunction;
function isMethod(obj) {
    return (typeof obj === "function") && ("$brand" in obj) && (obj["$brand"] === $PMethodBrand);
}
exports.isMethod = isMethod;
// TODO(alex): Will nothing always be value 'undefined'?
function isNothing(obj) { return obj === undefined; }
exports.isNothing = isNothing;
;
var isNumber = _NUMBER["isPyretNumber"];
exports.isNumber = isNumber;
var isRoughNumber = _NUMBER["isRoughnum"];
exports.isRoughNumber = isRoughNumber;
function isBoolean(val) {
    return typeof val === "boolean";
}
exports.isBoolean = isBoolean;
function isString(val) {
    return typeof val === "string";
}
exports.isString = isString;
function isDataVariant(val) {
    return (typeof val === "object") && ("$variant" in val);
}
exports.isDataVariant = isDataVariant;
function isRawObject(val) {
    return (typeof val === "object") && !("$variant" in val) && !(isPTuple(val)) && !(isTable(val)) && !(isRow(val));
}
exports.isRawObject = isRawObject;
function isPTuple(val) {
    return (Array.isArray(val)) && ("$brand" in val) && (val["$brand"] === $PTupleBrand);
}
exports.isPTuple = isPTuple;
function isArray(val) {
    return (Array.isArray(val)) && !("$brand" in val);
}
exports.isArray = isArray;
function isPRef(val) {
    return hasBrand($PRefBrand, val);
}
exports.isPRef = isPRef;
function makeMethodBinder(inner) {
    return function binder(pyretSelf) {
        var myFunction = function () {
            var innerArgs = [pyretSelf].concat(Array.from(arguments));
            return inner.apply(null, innerArgs);
        };
        myFunction["$brand"] = $PMethodBrand;
        myFunction["$binder"] = binder;
        return myFunction;
    };
}
exports.makeMethodBinder = makeMethodBinder;
function hasBrand(brand, val) {
    return (typeof val === "object") && ("$brand" in val) && (val["$brand"] === brand);
}
exports.hasBrand = hasBrand;
function applyBrand(brand, val) {
    val["$brand"] = brand;
    return val;
}
exports.applyBrand = applyBrand;
