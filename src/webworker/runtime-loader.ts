var runtimefiles = require("./runtime-files.json");
var BrowserFS = require("browserfs");

/*
BrowserFS.configure({
  fs: "LocalStorage"
}, function(e) {
  if (e) {
    throw e;
  }
});*/

var fs = BrowserFS.BFSRequire("fs");
function load(): void {
  let files = runtimefiles;

  let prewritten = "./prewritten";
  let uncompiled = "./uncompiled";
  if (fs.existsSync(prewritten) === false) {
    fs.mkdirSync(prewritten);
  }

  if (fs.existsSync(uncompiled) === false) {
    fs.mkdirSync(uncompiled);
  }

  for (var index in files) {
    var path = files[index].key;
    var content = files[index].content;
    var canonicalTimestamp = files[index].timestamp;
    if (fs.existsSync(path)) {
      let statResult = fs.statSync(path);
      let localTimestamp = statResult.mtime.getTime();
      if (localTimestamp < canonicalTimestamp) {
        fs.writeFileSync(path, content);
      }

    } else {
      fs.writeFileSync(path, content);
    }
  }
}

module.exports = load;
