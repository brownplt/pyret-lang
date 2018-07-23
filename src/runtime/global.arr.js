var runtime = require('./runtime.js')

export function print(v) {
  console.log(runtime);
  process.stdout.write(String(v));
}

