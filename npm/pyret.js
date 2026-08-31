#!/usr/bin/env node

const usage = require('command-line-usage');
const commandLineArgs = require('command-line-args');
const pyretClient = require('./client-lib');
const path = require('path');
const fs = require('fs');
const stripAnsi = require('strip-ansi');
const {version} = require("./package.json");

const compilerPath = path.join(__dirname, "pyret-lang", "build", "phaseA", "pyret.jarr");
// The TypeScript port of the compiler (pyret-lang/src/ts-compiler). Present
// when the package was built with PYRET_NPM_TS=1 (see build.sh); selected by
// --backend ts or PYRET_COMPILER=ts, the CLI analogue of code.pyret.org's
// ?compiler=ts flag.
const tsCompilerPath = path.join(__dirname, "pyret-lang", "build", "ts-compiler", "pyret.js");

const usages = [
  {
    header: `Pyret Command-line Interface v${version}`,
    content:
      `The {bold pyret} command compiles and runs Pyret programs. It helps manage a compile server that runs in the background to speed up compilation jobs, and manages state in a project's working directory to cache compiled files.`
  },
  {
    header: 'Basic Usage',
    content: [
      //'$ pyret --init',
      //"# Creates a {underline .pyret/} directory in the current directory and",
      //"# starts a compile server (if one isn't already running)",
      '  $ cat ahoy-world.arr',
      '  check: "Ahoy " + "world!" is "Ahoy world!" end',
      '  $ pyret {underline ahoy-world.arr}',
      '  Starting Parley server...',
      '  1/1 modules compiled',
      '  Looks shipshape, your test passed, mate!',
      '',
      '',
      '  This command compiled and ran {underline ahoy-world.arr}. The first time, this will take a few seconds as a server starts up in the background, in order to make future compiles fast.',

      '',
      '',
      '  It\'s worth noting that the file is compiled into a standalone JavaScript file with the {underline .jarr} extension:',
      '',

      '  $ node {underline ahoy-world.jarr}',
      '  Looks shipshape, your test passed, mate!',

      '',
      '',
      '  Most uses (e.g. for homework) only need to use the {bold pyret} command directly on {underline .arr} files, but there are several other options that can be provided.',

    ]
  },
  {
    header: 'Options',
    optionList: [
      {
        name: 'help',
        alias: 'h',
        description: 'Show this help message.'
      },
      {
        name: 'version',
        alias: 'v',
        description: "Print version information"
      },
      /*
      {
        name: 'init',
        alias: 'i',
        type: Boolean,
        description: "Performs two convenient setup tasks: creates a {underline .pyret/} directory in the current directory, and starts a server if one isn't running for this user already."
      },
      */
      {
        name: 'program',
        alias: 'p',
        typeLabel: "{underline <file>.arr}",
        defaultOption: true,
        description: "This is the default option, so using the flag is optional. Specifies the path to the program to compile (usually a .arr file). Will start a server if one isn't running, and will report an error if there is no {underline .pyret} in this directory or in any parent of this directory. Generates a standalone compiled file based on {bold --outfile}, and immediately executes it. The exit code is non-zero if the file fails to compile, and is the exit code of the executed program if it compiles successfully."
      },
      {
        name: 'outfile',
        alias: 'o',
        typeLabel: "{underline <file>.jarr}",
        description: "Specify the file to put the standalone compiled output into. The program can be re-run without re-compiling by using the {bold node} command. Defaults to the name of the {bold --program} with {underline .arr} replaced with {underline .jarr}."
      },
      {
        name: 'quiet',
        alias: 'q',
        description: "Don't show the progress indicator output like \"1/4 modules compiled\""
      },
      {
        name: 'perilous',
        description: "Compromises error semantics for speed. If the program has no errors, it will produce the same outputs and answers. Currently, this means eliding most annotation checks in compiled code and in libraries. Fine-grained control is intentionally not provided since the specifics of how this works may change, but in general it will do less error checking in exchange for faster execution, and not change the meaning of programs with no errors."
      },
      {
        name: 'type-check',
        alias: 'y',
        description: "Turn on the type-checker, and report errors found by the type checker as compilation errors."
      },
      {
        name: 'checks',
        alias: 'e',
        description: "Specify which checks to execute (all, none, main, or only:<pattern>) (default all). 'only:<pattern>' will only run checks whose names contain (anywhere) the given string. For example, `check \"cs1-assignment1-test1\": 2 + 2 is 4 end` would match --checks only:cs1-assignment1 or --checks only:assignment1"
      },
      {
        name: 'no-check-mode',
        alias: 'k',
        description: "Omit check blocks during compilation, and generate a standalone program that doesn't print testing information at all."
      },
      /*
      {
        name: 'clean',
        alias: 'n',
        type: Boolean,
        description: "Removes all compiled code in the {underline .pyret} directory. This can be necessary when Pyret updates to clear out stale compiled files."
      },
      */
      {
        name: 'shutdown',
        alias: 's',
        type: Boolean,
        description: "Shuts down the currently-running compile server (if any is running), by sending it a message over the specified or default {bold --port}"
      },
      {
        name: 'port',
        alias: 't',
        type: String,
        description: "Specify the path to a socket file to use to communicate with the server. Defaults to {underline /tmp/parley-<username>/comm.sock}."
      },
      {
        name: 'backend',
        typeLabel: "{underline pyret|ts}",
        description: "Which compiler backend to use (defaults to {bold pyret} or the PYRET_COMPILER environment variable if set). {bold ts} selects the TypeScript port of the compiler. An explicit {bold --compiler} path overrides this choice."
      },
    ]
  },
  {
    header: 'The .pyret/ Directory',
    content: [

      'The first time you run the {bold pyret} command in a directory, it creates a {underline .pyret/} directory there.',
      '',
      'This directory is used to store the compiled versions of individual {underline .arr} files in your project. They will appear in {underline .pyret/compiled}.',
      '',
      'Each directory in which you run {bold pyret} will have this sub-directory created.  In general, you should never need to look in or modify the directory. If you want to look at the innards of Pyret, you can check out these files.'

    ]
  },
  {
    header: 'Multiple and Support Files',
    content: [
      `If you've used code.pyret.org for a class, you may have support code that looks like`,
      "",
      "import shared-gdrive('something.arr', '<long-id-here>') as S",
      "",
      `You may also have imports with 'my-gdrive' for your own work.`,
      "",
      `These types of import aren't supported in the offline compiler. You can,
      however, get the source for these files, save them, and use 'file'
      imports to access the contents.`,
      "",
      "import file('something.arr') as S",
      "import file('my-tests.arr') as T",
      "",
      `The paths you use should be relative to the Pyret file that contains the
      import statements.`
    ]
  },
  {
    header: 'The Compile Server',
    content: [

      `The compiler will not run without a running server. The {bold pyret} command
      tries to connect to a compile server on the specified port on
      startup, and if it cannot, starts one before continuing. The server
      accepts requests to start and stop compile jobs, and to shut down, and
      sends messages indicating compile status and when the job is complete.`.replace("\n", ""),

      '',

      `The default mode of operation is to have a single compile server running
      per user (hence the default naming of the {bold --port}). This makes it
      simple to change directories to different projects and get the benefits
      of the server's quick responses. If multiple compile jobs are sent at the
      same time, they are queued and processed in FIFO order.`.replace("\n", ""),

      '',

      `This server is experimental, and likely can get stuck running, or lose
      track of its connection, so it's worth specifying what resources it uses
      so they can be cleaned up. The server that's started in the background
      does the work of creating the specified socket file, and its process
      should clean up that file when it's done or when {bold --shutdown} is
      used. You can also manually remove it (check
      {underline /tmp/parley-<username>/}) if something gets wedged. This is
      the only bit of state that the client and server use to communicate. The
      server runs a file called {underline pyret.jarr}, so if you want to
      check on potential runaway server processes this command can find them:`,

      '',

      '$ {bold ps} aux | {bold grep} {underline pyret.jarr}'


    ]
  },
  {
    header: 'Support and Contact',
    content: 'This interface is experimental. Feedback and issue reports at https://github.com/brownplt/pyret-lang/issues/new are most appreciated.'
  }
];

const optionDefinitions = [
  { name: 'help', alias: 'h', type: Boolean, group: 'meta', defaultValue: false },
  { name: 'version', alias: 'v', type: Boolean, group: 'meta', defaultValue: false },
  { name: 'quiet', alias: 'q', type: Boolean, group: 'meta' },
  { name: 'norun', alias: 'c', type: Boolean, group: "meta", defaultValue: false },

  // These options affect how the client starts up and communiates with the server
  { name: 'shutdown', alias: 's', type: Boolean, group: "client", defaultValue: false },
  { name: 'port', alias: 't', type: String, group: "client" },
//   { name: 'clean', type: Boolean, group: "client", defaultValue: false },

  { name: 'compiler', type: String, group: "client" },
  { name: 'backend', type: String, defaultValue: process.env.PYRET_COMPILER || 'pyret', group: "client" },
  { name: 'global-parley', type: String, defaultValue: "~/.parley/" },
  { name: 'local-parley', type: String, defaultValue: ".pyret" },

  { name: 'program', alias: 'p', type: String, group: "pyret-options", defaultOption: true },

  // These options are passed on to the compiler, and have no effect (yet)
  // on the client
  { name: 'base-dir', type: String, group: "pyret-options" },
  { name: 'outfile', alias: 'o', type: String, group: "pyret-options" },
  { name: 'require-config', type: String, group: "pyret-options" },
  { name: 'builtin-js-dir', type: String, multiple: true, group: "pyret-options" },
  { name: 'builtin-arr-dir', type: String, multiple: true, group: "pyret-options" },
  { name: 'allow-builtin-overrides', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'checks', defaultValue: 'all', alias: 'e', type: String, group: "pyret-options" },
  { name: 'checks-format', defaultValue: 'text', alias: 'f', type: String, group: "pyret-options" },
  { name: 'no-check-mode', alias: 'k', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'compiled-dir', type: String, group: "pyret-options" },
  { name: 'standalone-file', type: String, group: "pyret-options" },
  { name: 'deps-file', type: String, group: "pyret-options" },

  { name: 'perilous', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'type-check', alias: 'y', type: Boolean, group: "pyret-options", defaultValue: false },
];

let options;

// https://stackoverflow.com/a/20585886/2718315
function printUsage() {
  if(process.stdout.isTTY) {
    console.log(usage(usages));
  }
  else {
    console.log(stripAnsi(usage(usages)));
  }
}

function printVersion() {
  console.log(`Pyret Command-line Interface v${version}`);
}

try {
  options = commandLineArgs(optionDefinitions);
  if(options.meta.help) {
    printUsage();
    process.exit(0);
  }
  else if(options.meta.version) {
    printVersion();
    process.exit(0);
  }
}
catch(e) {
  printUsage();
  process.exit(1);
}

const backend = options.client.backend;
if(backend !== "pyret" && backend !== "ts") {
  console.error("--backend must be pyret or ts (got " + backend + ")");
  process.exit(1);
}
if(!options.client.compiler) {
  const resolved = backend === "ts" ? tsCompilerPath : compilerPath;
  options.client.compiler = resolved;
  options["_all"]["compiler"] = resolved;
}
if(backend === "ts" && !fs.existsSync(options.client.compiler)) {
  console.error("The ts backend is not available: " + options.client.compiler + " does not exist.");
  console.error("(Build it with `make ts-compiler` in the pyret-lang checkout, or reinstall a package built with PYRET_NPM_TS=1.)");
  process.exit(1);
}

// Default behavior: use ".jarr" to replace ".arr"
if(!options["pyret-options"]["outfile"] && options["pyret-options"]["program"]) {
  const programName = options["pyret-options"]["program"];
  const parleyName = options["_all"]["local-parley"];
  if(path.extname(programName) === ".arr") {
    const outPath = path.join(parleyName, path.basename(programName).slice(0, -4) + ".jarr");
    options["pyret-options"]["outfile"] = outPath;
  }
}

pyretClient.start(options);

