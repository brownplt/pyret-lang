if (process.argv.length === 4) {
  var r = require("requirejs");
  var phase = process.argv[3];
  r.config({
    paths: {
      trove: "../../build/" + phase + "/trove",
      js: "../../build/" + phase + "/js",
      compiler: "../../build/" + phase + "/arr/compiler"
    }
  });


  r(["profiling-run"], function (p) {});

} else {
  console.error('Usage: node profiling.js <file.arr> phase<X>');
}




