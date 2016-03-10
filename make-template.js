var mustache = require('mustache');
var file = require('fs');

var fileIn = process.argv[2];

function fileToString(f) {
  return String(file.readFileSync(f));
}

var fileContents = fileToString(fileIn);

var modules = {};
var mods = [
"arrays",
"contracts",
"either",
"error",
"error-display",
"equality",
"lists",
"option",
"pick",
"sets",
"srcloc",
"valueskeleton",

"ffi"
];

mods.forEach(function(m) {
  modules[m.replace("-", "_") + "_js"] = fileToString("build/phaseA/trove/" + m + ".js");
});

process.stdout.write(mustache.render(fileContents, modules));

