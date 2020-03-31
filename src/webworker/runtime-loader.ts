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
    const { path, content, timestamp } = item;

    const compiledPath = path.replace(/^prewritten/, 'compiled/builtin');

    if (fs.existsSync(path)) {
      const statResult = fs.statSync(path);
      const localTimestamp = statResult.mtime.getTime();
      if (localTimestamp < timestamp) {
        fs.writeFileSync(path, content);
        fs.writeFileSync(compiledPath, content);
      }
    } else {
      fs.writeFileSync(path, content);
      fs.writeFileSync(compiledPath, content);
    }
  });
}
