let files = require("../../build/worker/runtime-files.json");


function load(): void {
  let bfs = window["BrowserFS"];
  let fs = bfs.BFSRequire("fs");
  console.log("Loading files" , files);

  let prewritten = "./prewritten";
  let uncompiled = "./uncompiled";
  if (fs.existsSync(prewritten) === false) {
    fs.mkdirSync(prewritten);
  }

  if (fs.existsSync(uncompiled) === false) {
    fs.mkdirSync(uncompiled);
  }

  console.log("Loading files" , files);

  for (var index in files) {
    var path = files[index].key;
    var content = files[index].content;
    var canonicalTimestamp = files[index].timestamp;
    if (fs.existsSync(path)) {
      let statResult = fs.statSync(path);
      let localTimestamp = statResult.mtime.getTime();
      console.log(path, localTimestamp, canonicalTimestamp);
      if (localTimestamp < canonicalTimestamp) {
        fs.writeFileSync(path, content);
      }

    } else {
      fs.writeFileSync(path, content);
    }
  }
}

module.exports = load;
