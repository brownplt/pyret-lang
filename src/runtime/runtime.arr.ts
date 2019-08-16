const $EqualBrand = {"names":false};
const $NotEqualBrand = {"names":["reason","value1","value2"]};
const $UnknownBrand = {"names":["reason","value1","value2"]};
const $EqualTag = 0;
const $NotEqualTag = 1;
const $UnknownTag = 2;

type UndefBool = undefined | boolean

interface Equal { 
  $brand: any,
  $tag: number,
}

interface NotEqual {
  $brand: any,
  $tag: number,
  reason: string,
  value1: any,
  value2: any,
}

interface Unknown {
  $brand: any,
  $tag: number,
  reason: string,
  value1: any,
  value2: any,
}

type TypeEqualityResult = Equal | NotEqual | Unknown;

interface IEqualityResult {
  Equal: Equal,
  NotEqual: (reason: string, value1: any, value2: any) => NotEqual,
  Unknown: (reason: string, value1: any, value2: any) => Unknown,
  "is-Equal": (val: any) => boolean,
  "is-NotEqual": (val: any) => boolean,
  "is-Unknown": (val: any) => boolean,
}

const EqualityResult: IEqualityResult = {
  // TODO(alex): implement is-EqualityResult

  "Equal": {
    "$brand": $EqualBrand,
    "$tag": $EqualTag,
  },

  "NotEqual": function NotEqual(reason: string, value1: any, value2: any): NotEqual {
    return {
      "$brand": $NotEqualBrand,
      "$tag": $NotEqualTag,
      "reason": reason,
      "value1": value1,
      "value2": value2,
    };
  },

  "Unknown": function Unknown(reason: string, value1: any, value2: any): Unknown {
    return {
      "$brand": $UnknownBrand,
      "$tag": $UnknownTag,
      "reason": reason,
      "value1": value1,
      "value2": value2,
    };
  },

  "is-Equal": function Equal(val: any): boolean{
    return val.$brand === $EqualBrand;
  },

  "is-NotEqual": function NotEqual(val: any): boolean {
    return val.$brand === $NotEqualBrand;
  },

  "is-Unknown": function Unknown(val: any): boolean {
    return val.$brand === $UnknownBrand;
  }
};

function equalityResultToBool(ans: TypeEqualityResult): boolean {
  if (EqualityResult["is-Equal"](ans)) { 
    return true; 
  } else if (EqualityResult["is-NotEqual"](ans)) { 
    return false; 
  } else if (EqualityResult["is-Unknown"](ans)) {
    let unknownVariant = ans as Unknown;
    throw {
      reason: unknownVariant.reason,
      value1: unknownVariant.value1,
      value2: unknownVariant.value2,
    };
  }
}

function isFunction(obj: any): boolean { return typeof obj === "function"; }

// TODO(alex): Identify methods
function isMethod(obj: any): boolean { return typeof obj === "function"; }

// TODO(alex): Will nothing always be value 'undefined'?
function isNothing(obj: any): boolean { return obj === undefined };

// TODO(alex): Identify opaque types
function isOpaque(val: any) { return false; }

// TODO(alex): Handle Pyret numbers
function isNumber(val: any) {
   return typeof val === "number";
}


function identical3(v1: any, v2: any): TypeEqualityResult {
  if (isFunction(v1) && isFunction(v2)) {
    return EqualityResult.Unknown("Function", v1, v2);
  // TODO(alex): Handle/detect methods
  // } else if (isMethod(v1) && isMethod(v2)) {
  //  return thisRuntime.ffi.unknown.app('Methods', v1,  v2);
  //  TODO(alex): Handle/detect rough numbers
  // } else if (jsnums.isRoughnum(v1) && jsnums.isRoughnum(v2)) {
  //  return thisRuntime.ffi.unknown.app('Roughnums', v1,  v2);
  } else if (v1 === v2) {
    return EqualityResult.Equal;
  } else {
    return EqualityResult.NotEqual("", v1, v2);
  }
}

function identical(v1: any, v2: any): boolean {
  let ans: TypeEqualityResult = identical3(v1, v2);
  return equalityResultToBool(ans);
}

// equal3 :: PyretVal * PyretVal * Bool * (PyretNum U Undefined) * (Boolean U Undefined) * (Boolean U Undefined) -> EqualityResult
// JS function from Pyret values to Pyret equality answers

// Used for equal-always(3), equal-now(3), and within-(abs|rel)(3)

// left and right are two Pyret values to compare

// alwaysFlag is true for -always semantics (ref equality on mutables),
// false for -now semantics (cycle/deep equality on mutables)

// tol is the tolerance, expressed as a Pyret number (possibly an exact
// rational, possibly a roughnum).  For non-within calls, it isn't
// provided and is undefined.

// rel is a flag that indicates whether the tolerance should be
// interpreted as _absolute_ (two numbers are equal +/- tol) or _relative_
// (two numbers are equal +/- n * tol, where tol is between 0 and 1)

// fromWithin is a flag that indicates whether the tolerance came from
// a call to within() or one of its variants, or whether it was implicit
// (This affects the error message that gets generated)
function equal3(left: any, 
                right: any, 
                alwaysFlag: boolean, 
                tol: any, 
                rel: any, fromWithin: UndefBool): TypeEqualityResult {

  if(tol === undefined) { // means that we aren't doing any kind of within
    let isIdentical = identical3(left, right);
    if (!EqualityResult["is-NotEqual"](isIdentical)) {
      // if Equal or Unknown...
      return isIdentical;
    }
  }

  var stackOfToCompare = [];
  var toCompare = { stack: [], curAns: EqualityResult.Equal };
  var cache = {left: [], right: [], equal: []};
  function findPair(obj1, obj2) {
    for (var i = 0; i < cache.left.length; i++) {
      if (cache.left[i] === obj1 && cache.right[i] === obj2)
        return cache.equal[i];
    }
    return false;
  }
  function setCachePair(obj1, obj2, val) {
    for (var i = 0; i < cache.left.length; i++) {
      if (cache.left[i] === obj1 && cache.right[i] === obj2) {
        cache.equal[i] = val;
        return;
      }
    }
  }
  function cachePair(obj1, obj2) {
    cache.left.push(obj1);
    cache.right.push(obj2);
    cache.equal.push(EqualityResult.Equal);
    return cache.equal.length;
  }
  function equalHelp() {
    var current, curLeft, curRight;
    while (toCompare.stack.length > 0 && !EqualityResult["is-NotEqual"](toCompare.curAns)) {
      current = toCompare.stack.pop();
      if(current.setCache) {
        cache.equal[current.index - 1] = toCompare.curAns;
        continue;
      }
      curLeft = current.left;
      curRight = current.right;

      if (EqualityResult["is-Equal"](identical3(curLeft, curRight))) {
        continue;
      } else if (isNumber(curLeft) && isNumber(curRight)) {
        if (tol) {
          if (rel) {
            if (jsnums.roughlyEqualsRel(curLeft, curRight, tol, NumberErrbacks)) {
              continue;
            } else {
              toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
            }
          } else if (jsnums.roughlyEquals(curLeft, curRight, tol, NumberErrbacks)) {
            continue;
          } else {
            toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
          }
        } else if (jsnums.isRoughnum(curLeft) || jsnums.isRoughnum(curRight)) {
          toCompare.curAns = EqualityResult.Unknown(
            fromWithin ? "RoughnumZeroTolerances" : "Roughnums",
            curLeft,
            curRight);
        } else if (jsnums.equals(curLeft, curRight, NumberErrbacks)) {
          continue;
        } else {
          toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
        }
      } else if (isNothing(curLeft) && isNothing(curRight)) {
        continue;
      } else if (isFunction(curLeft) && isFunction(curRight)) {
        toCompare.curAns = EqualityResult.Unknown("Functions" , curLeft ,  curRight);
      } else if (isMethod(curLeft) && isMethod(curRight)) {
        toCompare.curAns = EqualityResult.Unknown("Methods" , curLeft , curRight);
      } else if (isOpaque(curLeft) && isOpaque(curRight)) {
        if (curLeft.equals(curLeft.val, curRight.val)) {
          continue;
        } else {
          toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
        }
      } else {
        var curPair = findPair(curLeft, curRight);
        if (curPair !== false) {
          // Already checked this pair of objects
          toCompare.curAns = curPair
          continue;
        } else {
          var index = cachePair(curLeft, curRight);
          toCompare.stack.push({ setCache: true, index: index, left: curLeft, right: curRight });
          if (isRef(curLeft) && isRef(curRight)) {
            if (alwaysFlag && !(isRefFrozen(curLeft) && isRefFrozen(curRight))) { // In equal-always, non-identical refs are not equal
              toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight); // We would've caught identical refs already
            } else if(!isRefSet(curLeft) || !isRefSet(curRight)) {
              toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
            } else { // In equal-now, we walk through the refs
              var newPath = current.path;
              var lastDot = newPath.lastIndexOf(".");
              var lastParen = newPath.lastIndexOf(")");
              if (lastDot > -1 && lastDot > lastParen) {
                newPath = newPath.substr(0, lastDot) + "!" + newPath.substr(lastDot + 1);
              } else {
                newPath = "deref(" + newPath + ")";
              }
              toCompare.stack.push({
                left: getRef(curLeft),
                right: getRef(curRight),
                path: newPath
              });
            }
          } else if(isTuple(curLeft) && isTuple(curRight)) {
            if (curLeft.vals.length !== curRight.vals.length) {
              toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
            } else {
              for (var i = 0; i < curLeft.vals.length; i++) {
                toCompare.stack.push({
                  left: curLeft.vals[i],
                  right: curRight.vals[i],
                  path: "is-tuple{ " + current.path + "; " + i + " }"
                });
              }
            }
          } else if (isArray(curLeft) && isArray(curRight)) {
            if (alwaysFlag || (curLeft.length !== curRight.length)) {
              toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
            } else {
              for (var i = 0; i < curLeft.length; i++) {
                toCompare.stack.push({
                  left: curLeft[i],
                  right: curRight[i],
                  path: "raw-array-get(" + current.path + ", " + i + ")"
                });
              }
            }
          } else if (isObject(curLeft) && isObject(curRight)) {
            if (!sameBrands(getBrands(curLeft), getBrands(curRight))) {
              /* Two objects with brands that differ */
              toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
            }
            else if (isObject(curLeft) && curLeft.dict["_equals"]) {
              /* Two objects with the same brands and the left has an _equals method */
              // If this call stack-returns,
              var newAns = getColonField(curLeft, "_equals").full_meth(curLeft, curRight, equalFunPy);
              if(isContinuation(newAns)) { return newAns; }
              // the continuation stacklet will get the result, and combine them manually
              toCompare.curAns = combineEquality(toCompare.curAns, newAns);
            }
            else if (isDataValue(curLeft) && isDataValue(curRight)) {
              /* Two data values with the same brands and no equals method on the left */
              var fieldNames = curLeft.$constructor.$fieldNames;
              if (fieldNames && fieldNames.length > 0) {
                for (var k = 0; k < fieldNames.length; k++) {
                  toCompare.stack.push({
                    left: curLeft.dict[fieldNames[k]],
                    right: curRight.dict[fieldNames[k]],
                    path: current.path + "." + fieldNames[k]
                  });
                }
              }
            } else {
              /* Two non-data objects with the same brands and no equals method on the left */
              var dictLeft = curLeft.dict;
              var dictRight = curRight.dict;
              var fieldsLeft;
              var fieldsRight;
              fieldsLeft = getFields(curLeft);
              fieldsRight = getFields(curRight);
              if(fieldsLeft.length !== fieldsRight.length) {
                toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
              }
              for(var k = 0; k < fieldsLeft.length; k++) {
                toCompare.stack.push({
                  left: curLeft.dict[fieldsLeft[k]],
                  right: curRight.dict[fieldsLeft[k]],
                  path: current.path + "." + fieldsLeft[k]
                });
              }
            }
          } else {
            toCompare.curAns = EqualityResult.NotEqual(current.path, curLeft, curRight);
          }
        }
      }
    }
    return toCompare.curAns;
  }
  var stackFrameDesc = [alwaysFlag ? "runtime equal-always" : "runtime equal-now"];
  function equalFun($ar) {
    var $step = 0;
    var $ans = undefined;
    if (thisRuntime.isActivationRecord($ar)) {
      $step = $ar.step;
      $ans = $ar.ans;
    }
    while(true) {
      switch($step) {
      case 0:
        $step = 1;
        $ans = equalHelp();
        if(isContinuation($ans)) {
          $ans.stack[thisRuntime.EXN_STACKHEIGHT++] = thisRuntime.makeActivationRecord(
            stackFrameDesc,
            equalFun,
            $step,
            [],
            []);
        }
        return $ans;
      case 1:
        toCompare.curAns = combineEquality(toCompare.curAns, $ans);
        $step = 0;
        break;
      }
    }
  }
  function reenterEqualFun(left, right) {
    // arity check
    var $step = 0;
    var $ans = undefined;
    if (thisRuntime.isActivationRecord(left)) {
      $step = left.step;
      $ans = left.ans;
    }
    while(true) {
      switch($step) {
      case 0:
        stackOfToCompare.push(toCompare);
        toCompare = {stack: [{left: left, right: right, path: "the-value"}], curAns: EqualityResult.Equal};
        $step = 1;
        $ans = equalFun();
        if(isContinuation($ans)) {
          $ans.stack[thisRuntime.EXN_STACKHEIGHT++] = thisRuntime.makeActivationRecord(
            stackFrameDesc,
            reenterEqualFun,
            $step,
            [],
            []);
          return $ans;
        }
        break;
      case 1:
        for(var i = 0; i < toCompare.stack.length; i++) {
          var current = toCompare.stack[i];
          if(current.setCache) {
            cache.equal[current.index - 1] = $ans;
          }
        }
        toCompare = stackOfToCompare.pop();
        return $ans;
      }
    }
  }
  var equalFunPy = makeFunction(reenterEqualFun, "equalFun");
  return reenterEqualFun(left, right);

}


function py_equal(e1, e2) {
  if(e1 === e2) { return true; }
  var worklist = [[e1,e2]];
  while(worklist.length > 0) {
    var curr = worklist.pop();
    var v1 = curr[0];
    var v2 = curr[1];
    if(v1 === v2) { continue; }
    if(v1.$brand && v1.$brand === v2.$brand) {
      var fields1 = v1.$brand.names;
      var fields2 = v2.$brand.names;
      if(fields1.length !== fields2.length) { return false; }
      for(var i = 0; i < fields1.length; i += 1) {
        if(fields1[i] != fields2[i]) { return false; }
        worklist.push([v1[fields1[i]], v2[fields2[i]]]);
      }
      continue;
    }
    return false;
  }
  return true;
}

function traceValue(loc, value) {
  // NOTE(alex): stubbed out until we decide what to actually do with it
  return value;
}

module.exports = {
  py_equal: py_equal,
  "trace-value": traceValue,
  "Equal": EqualityResult["Equal"],
  "NotEqual": EqualityResult["NotEqual"],
  "Unknown": EqualityResult["Unknown"],
  "is-Equal": EqualityResult["is-Equal"],
  "is-NotEqual": EqualityResult["is-NotEqual"],
  "is-Unknown": EqualityResult["is-Unknown"],
};
