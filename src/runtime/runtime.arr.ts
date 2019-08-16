const $EqualBrand = {"names":false};
const $NotEqualBrand = {"names":["reason","value1","value2"]};
const $UnknownBrand = {"names":["reason","value1","value2"]};
const $EqualTag = 0;
const $NotEqualTag = 1;
const $UnknownTag = 2;

const EqualityResult = {
  // TODO(alex): implement is-EqualityResult

  "Equal": {
    "$brand": $EqualBrand,
    "$tag": $EqualTag,
  },

  "NotEqual": function NotEqual(reason, value1, value2) {
    return {
      "$brand": $NotEqualBrand,
      "$tag": $NotEqualTag,
      "reason": reason,
      "value1": value1,
      "value2": value2,
    };
  },

  "Unknown": function Unknown(reason, value1, value2) {
    return {
      "$brand": $UnknownBrand,
      "$tag": $UnknownTag,
      "reason": reason,
      "value1": value1,
      "value2": value2,
    };
  },

  "is-Equal":function Equal(val) {
    return val.$brand === $EqualBrand;
  },

  "is-NotEqual":function NotEqual(val) {
    return val.$brand === $NotEqualBrand;
  },

  "is-Unknown":function Unknown(val) {
    return val.$brand === $UnknownBrand;
  }
};

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
