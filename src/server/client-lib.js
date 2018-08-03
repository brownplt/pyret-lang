const WebSocket = require('ws');
const childProcess = require('child_process');
const fs = require('fs');
const path = require('path');
const mkdirp = require('mkdirp');
const os = require('os');

function findLocalParleyDir(base, localParley) {
  if(fs.existsSync(path.resolve(path.join(base, localParley)))) {
    return path.resolve(path.join(base, localParley));
  }
  return false;
}

/*
  The tmpdir is used to store the pid file and the socket

  TODO: windows
*/
function tmpdir() {
  const name = "parley-anchor" + os.userInfo().username;
  const fulldir = "/tmp/" + name;
  if(!fs.existsSync(fulldir)) {
    try {
      mkdirp.sync(fulldir);
    }
    catch(e) {
      error("Could not find or create temporary directory " + fulldir);
      process.exit(1);
    }
  }
  return fulldir; 
}

function getSocket() {
  const dir = tmpdir();
  const portFile = path.join(dir, "comm.sock");
  return portFile;
}

function start(options) {

  const LOG = 4; // For debugging
  const INFO = 3; // Here and below are for the user
  const WARN = 2;
  const ERROR = 1;
  const SILENT = 0;
  var LOG_LEVEL = INFO;
  if(options.meta.quiet) { LOG_LEVEL = ERROR; }

  function makeLogger(level) {
    return function(...args) {
      if(LOG_LEVEL >= level) {
        console.log.apply(console, ["[client] ", new Date()].concat(args));
      }
    }
  }

  const info = makeLogger(INFO);
  const log = makeLogger(LOG);
  const warn = makeLogger(WARN);
  const error = makeLogger(ERROR);
  
  const localParley = options["_all"]["local-parley"];
  var localParleyDir = findLocalParleyDir(process.cwd(), localParley);

  if(localParleyDir === false) {
    try {
      mkdirp.sync(localParley);
      localParleyDir = path.resolve(path.join(process.cwd(), localParley));
    }
    catch(e) {
      error(String(e));
      error("No " + localParley + " directory found and couldn't create it, exiting");
      process.exit(1);
    }
  }
  
  const serverModule = options.client.compiler;
  var portFile = getSocket();

  if(options.client.port) {
    portFile = path.resolve(options.client.port); // Allow user to override location of port file
  }

  if(!options["pyret-options"]["compiled-dir"]) {
    const compileTarget = path.join(localParleyDir, "compiled");
    options["pyret-options"]["compiled-dir"] = compileTarget;
  }
  if(!options["pyret-options"]["base-dir"]) {
    const baseDir = path.resolve(path.join(localParleyDir, ".."));
    options["pyret-options"]["base-dir"] = baseDir;
  }

  function shutdown() {
    try {
      const client = new WebSocket("ws+unix://" + portFile);
      client.on('error', function(err) {
        error('Connection error: ' + err.toString() + ".");
        error('You can try again, and the Pyret server may restart. If you see this error repeatedly, report it as a bug.');
      });
      client.on('open', function(connection) {
        client.send(JSON.stringify({ command: 'shutdown' }));
      });
      
      tryToRemoveFiles();
    }
    catch(e) {
      warn("Error during shutdown: " + e);
      tryToRemoveFiles();
    }
  }

  function tryToRemoveFiles() {
    if(fs.existsSync(portFile)) { fs.unlinkSync(portFile); }
  }

  if(options.client.shutdown) {
    shutdown();
    return;
    //process.exit(0);
  }

  function runProgram(path) {
    log("Executing program: ", path);
    const proc = childProcess.spawn("node", [path], {stdio: 'inherit'});
    proc.on('close', function(code) {
      process.exit(code);
    });
  }

  function makeSocketAndConnect() {
    const client = new WebSocket("ws+unix://" + portFile);
    client.on('error', function(err) {
      error('Connection error: ' + err.toString() + ".");
      error('You can try again, and the Pyret server may restart. If you see this error repeatedly, report it as a bug.');
      shutdown();
    });

    function sigint() {
      log("Caught interrupt signal during compile job, stopping compile job");
      client.send(JSON.stringify({ command: 'stop' }));
      process.exit(0);
    }

    client.on('close', function() {
      log('parley connection closed');
      process.removeListener('SIGINT', sigint);
    });
     
    client.on('open', function(connection) {
      log('parley protocol connected');

      process.on('SIGINT', sigint);

      client.on('message', function(message) {
        const parsed = JSON.parse(message);
        if(parsed.type === 'echo-log') {
          if(options.meta.quiet) { return; }
          if(parsed["clear-first"]) {
            process.stdout.write("\r");
            process.stdout.write(new Array(parsed["clear-first"] + 1).join(" "));
            process.stdout.write("\r");
            process.stdout.write(parsed.contents);
          }
          else {
            process.stdout.write(parsed.contents);
          }
        }
        else if(parsed.type === 'echo-err') {
          process.stderr.write(parsed.contents);
        }
        else if(parsed.type === "compile-failure") {
          // Intentional no-op
        }
        else if(parsed.type === "compile-success") {
          log("Successful compile response");
          if(!options.meta.norun) {
            process.nextTick(() => runProgram(options["pyret-options"]["outfile"]));
          }
        }
      });


      var forMessage = { command: "compile", compileOptions: JSON.stringify(options['pyret-options']) };
      client.send(JSON.stringify(forMessage));
    });
    return client;
  }


  function startupServer(port, wait) {
    const child = childProcess.fork(
      serverModule,
      ["-serve", "--port", port],
      {
        stdio: [0, 1, 2, 'ipc'],
        execArgv: ["-max-old-space-size=8192"]
      } // To send messages on completion of startup
    );

    if(wait) {
      return new Promise((resolve, reject) => {
        child.on('message', function(msg) {
          if(msg.type === 'success') {
            child.unref();
            child.disconnect();
            resolve(msg);
          }
          else {
            reject(msg);
          }
        });
      });
    }
    else {
      child.unref();
      child.disconnect();
    }
  }

  const connectURL = portFile;
  if (fs.existsSync(portFile)) {
    // If the compiler is newer than the server process, restart
    var compilerStats = fs.lstatSync(serverModule);
    var portStats = fs.lstatSync(portFile);
    if(portStats.mtime.getTime() < compilerStats.mtime.getTime()) {
      info("A running server was found, but the chosen compiler (" + serverModule + ") is newer than the server.  Restarting...");
      shutdown();
      startupServer(portFile, true)
        .then(() => makeSocketAndConnect())
        .catch((err) => error('Starting up the server failed: ', err));
    }
    else {
      log("Connecting to: ", connectURL);
      makeSocketAndConnect();
    }
  // Otherwise, try starting up a server and waiting for it, then doing the work
  } else {
    if (fs.existsSync(portFile)) {
      makeSocketAndConnect();
    }
    else {
      info("Starting up server...");
      startupServer(portFile, true)
        .then(() => makeSocketAndConnect())
        .catch((err) => console.error('Starting up the server failed: ', err));
    }
  }

}

module.exports = {
  start: start
}
