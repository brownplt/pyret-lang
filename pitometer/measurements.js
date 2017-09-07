define(["./bench-programs", "./time-phases", "./count-lines"], function(bp, tp, cl) {

  return {
    "bench-programs": bp.run,
    "time-phases": tp.run,
    "count-lines": cl.run,
//    "count-ar-vars": cav.run
  };

});

