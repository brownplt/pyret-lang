/*jshint globalstrict: true, node: true
*/
"use strict";

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

// Necessary to work with stopify
return module.exports = {
  py_equal: py_equal,
  "trace-value": traceValue,
};
