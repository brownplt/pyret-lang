const commandLineArgs = require('command-line-args');
const pyretClient = require('./client-lib');
 
const optionDefinitions = [
  // These options affect how the client starts up and communiates with the server
  { name: 'shutdown', alias: 's', type: Boolean, group: "client" },
  { name: 'port', alias: 't', type: String, defaultValue: "1701", group: "client" },
  { name: 'compiler', type: String, defaultValue: "build/phaseA/pyret.jarr", group: "client" },

  // These options are passed on to the compiler, and have no effect (yet)
  // on the client
  { name: 'program', type: String, group: "pyret-options" },
  { name: 'norun', type: Boolean, group: "pyret-options" },
  { name: 'outfile', type: String, group: "pyret-options" },
  { name: 'require-config', type: String, group: "pyret-options" },
  { name: 'builtin-js-dir', type: String, group: "pyret-options" },
  { name: 'builtin-arr-dir', type: String, group: "pyret-options" },
  { name: 'checks', alias: 'e', type: String, group: "pyret-options" },
  { name: 'no-check-mode', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'compiled-dir', type: String, group: "pyret-options" },
  { name: 'standalone-file', type: String, group: "pyret-options" }
  { name: 'bundle-dependencies', type: Boolean, group: "pyret-options" }

];

const options = commandLineArgs(optionDefinitions);

pyretClient.start(options);

