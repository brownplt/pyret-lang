const client = require('../src/server/client-lib.js');
const path = require('path');

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
    'type-check': true,
    'checks': 'none',
    'program': process.argv[2],
    'builtin-js-dir': 'build/runtime',
    'base-dir': path.resolve(path.join(__dirname, "/../"))
  }
});
