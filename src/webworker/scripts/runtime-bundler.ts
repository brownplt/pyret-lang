var fs = require("fs");
var path = require("path");

function walkSync(dir, base_key) {
  let files = fs.readdirSync(dir);
  let filelist = [];

  files.forEach(function(file) {
    let statResult = fs.statSync(path.join(dir, file));
    if (fs.statSync(path.join(dir, file)).isDirectory()) {
      let recPath = path.join(dir, file);
      let recKey = path.join(base_key, file);
      let recResult = walkSync(recPath, recKey);
      filelist = filelist.concat(recResult);
    } else {
      let key = path.join(base_key, file);
      let filePath = path.join(dir, file);
      let content = fs.readFileSync(filePath).toString();
      filelist.push({ key: key, content: content});
    }
  });

  return filelist;

}

var prewrittenBuiltinsDir = process.argv[2];
var uncompiledBuiltinsDir = process.argv[3];
var outputFile = process.argv[4];

var prewrittenObject = walkSync(prewrittenBuiltinsDir, ".prewritten");
var uncompiledObject = walkSync(uncompiledBuiltinsDir, ".uncompiled");

var fileObject = prewrittenObject.concat(uncompiledObject);
var fileString = JSON.stringify(fileObject);

fs.writeFileSync(outputFile, fileString);
