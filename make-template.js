var mustache = require('mustache');
var file = require('fs');

var fileIn = process.argv[2];

function fileToString(f) {
  return String(file.readFileSync(f));
}

var fileContents = fileToString(fileIn);

var option = fileToString("build/phaseA/trove/option.js");
var justOpt = fileToString("build/phaseA/trove/just-option.js");

process.stdout.write(mustache.render(fileContents, {
  OPTION: option,
  JUSTOPTION: justOpt
}));

