import { compiled, compiledBuiltin } from './path';

export const load = (
  fs: any,
  prewrittenDirectory: string,
  uncompiledDirectory: string,
  runtimeFiles: any): void => {
  if (!fs.existsSync(prewrittenDirectory)) {
    fs.mkdirSync(prewrittenDirectory);
  }

  if (!fs.existsSync(compiled)) {
    fs.mkdirSync(compiled);
  }

  if (!fs.existsSync(compiledBuiltin)) {
    fs.mkdirSync(compiledBuiltin);
  }

  for (var index in runtimeFiles) {
    var path = runtimeFiles[index].key;
    var content = runtimeFiles[index].content;
    var canonicalTimestamp = runtimeFiles[index].timestamp;

    const compiledPath = path.replace(/^prewritten/, "compiled/builtin");

    if (fs.existsSync(path)) {
      let statResult = fs.statSync(path);
      let localTimestamp = statResult.mtime.getTime();
      if (localTimestamp < canonicalTimestamp) {
        fs.writeFileSync(path, content);
        fs.writeFileSync(compiledPath, content);
      }

    } else {
      fs.writeFileSync(path, content);
      fs.writeFileSync(compiledPath, content);
    }
  }
};
