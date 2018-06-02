const stopify = require('stopify/dist/src/stopify/compileFunction');
const fs = require('fs');
const path = require('path');

const defaultOpts = {
  getters: false,
  compileFunction: true,
  debug: false,
  captureMethod: 'lazy',
  newMethod: 'wrapper',
  es: 'sane',
  hofs: 'builtin',
  jsArgs: 'simple',
  requireRuntime: false,
  noWebpack: true
};

const runtime_path = path.join(
  __dirname, '/src/js/base/stopified-vhull-runtime.original.js')
const runtime_func = fs.readFileSync(runtime_path).toString()

const compiled = stopify.compileFunction(runtime_func, defaultOpts)

const stopified_toWrite =
  `define("pyret-base/js/stopified-vhull-runtime", ${compiled});`

const original_toWrite =
  `define("pyret-base/js/stopified-vhull-runtime", ${runtime_func});`

const stopfied_out = path.join(
  __dirname, '/src/js/base/stopified-vhull-runtime.js')

const original_out = path.join(
  __dirname, '/build/phaseA/js/stopified-vhull-runtime.original.js')

fs.writeFileSync(stopfied_out, stopified_toWrite)
fs.writeFileSync(original_out, original_toWrite)
