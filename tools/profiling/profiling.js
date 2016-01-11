if (process.argv.length === 3) {
  r = require("requirejs");
  
  r.config({
    paths: {
      trove: "../../build/phase1/trove",
      js: "../../build/phase1/js",
      compiler: "../../build/phase1/arr/compiler"
    }
  });


  r(["profiling-run"], function (p) {});

} else {
  console.error('Usage: node profiling.js <file.arr>');
}




