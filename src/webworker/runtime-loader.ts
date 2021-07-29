import { compiled, compiledBuiltin } from './path';

export default function load(
  fs: any,
  path: any,
  prewrittenDirectory: string,
  uncompiledDirectory: string,
  runtimeFiles: any,
): void {
  if (!fs.existsSync(prewrittenDirectory)) {
    fs.mkdirSync(prewrittenDirectory);
  }

  if (!fs.existsSync(compiled)) {
    fs.mkdirSync(compiled);
  }

  if (!fs.existsSync(compiledBuiltin)) {
    fs.mkdirSync(compiledBuiltin);
  }

  if (!fs.existsSync('/tmp')) {
    fs.mkdirSync('/tmp');
  }

  runtimeFiles.forEach((item: any) => {
    const { key, content, timestamp } = item;

    const fullPathKey = `/${key}`;

    const compiledPath = fullPathKey.replace(/^\/prewritten/, '/compiled/builtin');

    if (fs.existsSync(fullPathKey)) {
      const statResult = fs.statSync(fullPathKey);
      const localTimestamp = statResult.mtime.getTime();
      if (localTimestamp < timestamp) {
        if (!fs.existsSync(path.dirname(compiledPath))) {
          fs.mkdirSync(path.dirname(compiledPath));
        }
        fs.writeFileSync(fullPathKey, content);
        fs.writeFileSync(compiledPath, content);
      }
    } else {
      if (!fs.existsSync(path.dirname(fullPathKey))) {
        fs.mkdirSync(path.dirname(fullPathKey));
        fs.mkdirSync(path.dirname(compiledPath));
      }
      fs.writeFileSync(fullPathKey, content);
      fs.writeFileSync(compiledPath, content);
    }
  });
}
