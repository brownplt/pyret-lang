var fs = require("fs");
var path = require("path");

function walkSync(dir, base_key) {
  let files = fs.readdirSync(dir);
  let filelist = [];

  files.forEach(function(file) {
    let statResult = fs.statSync(path.join(dir, file));
    if (statResult.isDirectory()) {
      let recPath = path.join(dir, file);
      let recKey = path.join(base_key, file);
      let recResult = walkSync(recPath, recKey);
      filelist = filelist.concat(recResult);
    } else {
      let key = path.join(base_key, file);
      let filePath = path.join(dir, file);
      let content = fs.readFileSync(filePath).toString();
      filelist.push({ key: key, content: content, timestamp: statResult.mtime.getTime()});
    }
  });

  return filelist;

}

function checkDirectory(p) {
  if (fs.existsSync(p) == false) {
    // console.error("Unabled to bundle runtime files.", p, "does not exist");
    throw ("Unable to bundle runtime files. " + p + " does not exist");
  } 


  let statResult = fs.statSync(p);
  if (statResult.isDirectory() == false) {
    // console.error("Unable to bundle runtime files.", p, "is not a directory");
    throw ("Unable to bundle runtime files. " + p + " is not a directory");
  }

  return true;
}

var prewrittenBuiltinsDir = process.argv[2];
var outputFile = process.argv[3];

var prewrittenObject = [];

if (checkDirectory(prewrittenBuiltinsDir)) {
  prewrittenObject = walkSync(prewrittenBuiltinsDir, "./prewritten");
}

var filelist = prewrittenObject;

var output = JSON.stringify(filelist);

fs.writeFileSync(outputFile, output);
