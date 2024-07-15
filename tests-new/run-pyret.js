const client = require('../src/server/client-lib.js');
const path = require('path');

const [filename, typecheck, pipeline, session] = process.argv.slice(2);

client.start({
  client: {
    port: "tests-new/pyret-tests.sock",
    compiler: "build/phaseA/pyret.jarr",
  },
  _all: {
    'local-parley': "tests-new/.pyret"
  },
  meta: {
    norun: true
  },
  "pyret-options": {
    'type-check': typecheck === "typecheck",
    'pipeline': pipeline,
    'program': filename,
    'builtin-js-dir': 'build/runtime',
    'base-dir': path.resolve(path.join(__dirname, "/../")),
    'session': session || "empty",
    'recompile-builtins': false
  }
});
