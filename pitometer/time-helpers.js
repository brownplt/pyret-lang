define([], function() {
  function maybeTime(shouldRun, f) {
    if(!shouldRun) { return [false, "skipped", 0]; }
    var start = process.hrtime();
    try {
      var ans = f();
      return [true, ans, process.hrtime(start)];
    }
    catch(e) {
      return [false, String(e), process.hrtime(start)]
    }
  }

  function hrtimeToMicroseconds(t) {
    return (t[0] * 1000000) + (t[1] / 1000);
  }
  
  return {
    maybeTime: maybeTime,
    hrtimeToMicroseconds: hrtimeToMicroseconds
  };
});
