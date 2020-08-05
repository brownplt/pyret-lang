import { compiled, compiledBuiltin } from './path';

export default function load(
  fs: any,
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

  runtimeFiles.forEach((item: any) => {
    const { key, content, timestamp } = item;

    const fullPathKey = `/${key}`;

    const compiledPath = fullPathKey.replace(/^\/prewritten/, '/compiled/builtin');

    if (fs.existsSync(fullPathKey)) {
      const statResult = fs.statSync(fullPathKey);
      const localTimestamp = statResult.mtime.getTime();
      if (localTimestamp < timestamp) {
        fs.writeFileSync(fullPathKey, content);
        fs.writeFileSync(compiledPath, content);
      }
    } else {
      fs.writeFileSync(fullPathKey, content);
      fs.writeFileSync(compiledPath, content);
    }
  });
}
