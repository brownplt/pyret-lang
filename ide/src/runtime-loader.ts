const runtimeFiles = require("./runtime-files.json");

function load(bfs: any, prewrittenDirectory: any): void {
  let fs = bfs.BFSRequire("fs");

  let prewritten = prewrittenDirectory;
  if (fs.existsSync(prewritten) === false) {
    fs.mkdirSync(prewritten);
  }

  for (var index in runtimeFiles) {
    var path = runtimeFiles[index].key;
    var content = runtimeFiles[index].content;
    var canonicalTimestamp = runtimeFiles[index].timestamp;
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
