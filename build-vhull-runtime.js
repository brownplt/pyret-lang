const Stopify = require('Stopify');
const fs = require('fs');
const path = require('path');

const runtime_path = path.join(
  __dirname, '/src/js/base/stopified-vhull-runtime.original.js')
const runtime_func = fs.readFileSync(runtime_path).toString()

const compiled = Stopify.compileFunction(runtime_func)

const toWrite = `define("pyret-base/js/stopified-vhull-runtime", ${compiled});`

const out = path.join(
  __dirname, '/src/js/base/stopified-vhull-runtime.js')

fs.writeFileSync(out, toWrite)
