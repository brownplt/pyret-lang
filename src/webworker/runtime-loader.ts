const runtimeFiles = require("../../build/worker/runtime-files.json");


function load(bfs, prewrittenDirectory, uncompiledDirectory): void {
  let fs = bfs.BFSRequire("fs");

  let prewritten = prewrittenDirectory;
  if (fs.existsSync(prewritten) === false) {
    fs.mkdirSync(prewritten);
  }

  // TODO(alex): uncompiled no longer used
  let uncompiled = uncompiledDirectory;
  if (fs.existsSync(uncompiled) === false) {
    fs.mkdirSync(uncompiled);
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
