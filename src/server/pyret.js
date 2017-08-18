#!/usr/bin/env node

const commandLineArgs = require('command-line-args');
const pyretClient = require('./client-lib');
const path = require('path');

const optionDefinitions = [
  // These options affect how the client starts up and communiates with the server
  { name: 'shutdown', alias: 's', type: Boolean, group: "client", defaultValue: false },
  { name: 'port', alias: 't', type: String, group: "client" },
  { name: 'compiler', alias: 'c', type: String, defaultValue: "node_modules/pyret-lang/build/phaseA/pyret.jarr", group: "client" },
  { name: 'global-parley', type: String, defaultValue: "~/.parley/" },
  { name: 'local-parley', type: String, defaultValue: ".pyret" },

  { name: 'program', type: String, group: "pyret-options", defaultOption: true },

  // These options are passed on to the compiler, and have no effect (yet)
  // on the client
  { name: 'outfile', type: String, group: "pyret-options" },
  { name: 'require-config', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/scripts/node_modules-config.json" },
  { name: 'builtin-js-dir', type: String, multiple: true, group: "pyret-options", defaultValue: ["node_modules/pyret-lang/src/js/trove"] },
  { name: 'builtin-arr-dir', type: String, multiple: true, group: "pyret-options", defaultValue: ["node_modules/pyret-lang/src/arr/trove"] },
  { name: 'allow-builtin-overrides', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'no-check-mode', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'compiled-dir', type: String, group: "pyret-options" },
  { name: 'standalone-file', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/js/base/handalone.js" },
  { name: 'deps-file', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/build/bundled-node-deps.js" }

];

const options = commandLineArgs(optionDefinitions);

// Default behavior: use ".jarr" to replace ".arr"
if(!options["pyret-options"]["outfile"] && options["pyret-options"]["program"]) {
  const programName = options["pyret-options"]["program"];
  if(path.extname(programName) === ".arr") {
    options["pyret-options"]["outfile"] = programName.slice(0, -4) + ".jarr";
  }
}

pyretClient.start(options);

