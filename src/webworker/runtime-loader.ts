const load = (
  fs: any,
  prewrittenDirectory: string,
  uncompiledDirectory: string,
  runtimeFiles: any): void => {

  if (!fs.existsSync(prewrittenDirectory)) {
    fs.mkdirSync(prewrittenDirectory);
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

module.exports = {
  load: load
};
