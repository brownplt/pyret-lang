const WebSocket = require('ws');
const childProcess = require('child_process');
const fs = require('fs');
const path = require('path');
const mkdirp = require('mkdirp');
const os = require('os');

const INFO = 4;
const LOG = 3;
const WARN = 2;
const ERROR = 1;
const SILENT = 0;
var LOG_LEVEL = WARN;

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

function findLocalParleyDir(base, localParley) {
  if(path.resolve(base) === "/") { return false; }
  if(fs.existsSync(path.resolve(path.join(base, localParley)))) {
    return path.resolve(path.join(base, localParley));
  }
  else {
    return findLocalParleyDir(path.join(base, ".."), localParley);
  }
}

/*
  The tmpdir is used to store the pid file and the socket

  TODO: windows
*/
function tmpdir() {
  const name = "parley-" + os.userInfo().username;
  const fulldir = "/tmp/" + name;
  if(!fs.exists(fulldir)) {
    try {
      mkdirp.sync(fulldir);
    }
    catch(e) {
      error("Could not find or create temporary directory " + fulldir);
    }
  }
  return fulldir; 
}

function getSockAndPIDFile() {
  const dir = tmpdir();
  const portFile = path.join(dir, "comm.sock");
  const pidFile = path.join(dir, "pid");
  return {
    portFile: portFile,
    pidFile: pidFile
  };
}

function start(options) {
  
  const localParley = options["_all"]["local-parley"];
  const localParleyDir = findLocalParleyDir(process.cwd(), localParley);

  if(localParleyDir === false) {
    try {
      mkdirp.sync(localParley);
      localParleyDir = path.resolve(path.join(process.cwd(), localParley));
    }
    catch(e) {
      error("No " + localParley + " directory found and couldn't create it, exiting");
      process.exit(1);
    }
  }

  const serverModule = options.client.compiler;
  var {portFile, pidFile} = getSockAndPIDFile();

  if(options.client.port) {
    portFile = path.resolve(options.client.port); // Allow user to override location of port file
  }

  if(!options["pyret-options"]["compiled-dir"]) {
    const compileTarget = path.join(localParleyDir, "compiled");
    options["pyret-options"]["compiled-dir"] = compileTarget;
  }

  function shutdown() {
    try {
      const pid = Number(fs.readFileSync(pidFile));
      process.kill(pid, 'SIGINT');
      fs.unlinkSync(pidFile);
      fs.unlinkSync(portFile);
      info("Sent kill signal to " + pid);
    }
    catch(e) {
      info("No process to quit: " + e);
      fs.unlinkSync(pidFile);
      fs.unlinkSync(portFile);
    }
  }

  if(options.client.shutdown) {
    shutdown();
    process.exit(0);
  }

  function makeSocketAndConnect() {
    const client = new WebSocket("ws+unix://" + portFile);
    client.on('error', function(err) {
      error('Connection error: ' + err.toString() + ".");
      error('You can try again, and the Pyret server may restart. If you see this error repeatedly, report it as a bug.');
      shutdown();
    });

    client.on('close', function() {
      info('parley connection closed');
    });
     
    client.on('open', function(connection) {
      info('parley protocol connected');
      client.on('message', function(message) {
        const parsed = JSON.parse(message);
        if(parsed.type === 'echo-log') {
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
      });
      client.send(JSON.stringify(options['pyret-options']));
    });
    return client;
  }


  function startupServer(port) {
    const child = childProcess.fork(
      serverModule,
      ["-serve", "--port", port],
      {
        stdio: [0, 1, 2, 'ipc'],
        execArgv: ["-max-old-space-size=8192"]
      } // To send messages on completion of startup
    );

    return new Promise((resolve, reject) => {
      child.on('message', function(msg) {
        if(msg.type === 'success') {
          child.unref();
          child.disconnect();
          fs.writeFileSync(pidFile, String(child.pid));
          resolve(msg);
        }
        else {
          reject(msg);
        }
      });
    });
  }

  const connectURL = portFile;
  if (fs.existsSync(pidFile)) {
    // If the compiler is newer than the server process, restart
    var compilerStats = fs.lstatSync(serverModule);
    var pidStats = fs.lstatSync(pidFile);
    if(pidStats.mtime.getTime() < compilerStats.mtime.getTime()) {
      info("A running server was found, but the chosen compiler (" + serverModule + ") is newer than the server.  Restarting...");
      shutdown();
      startupServer(portFile)
        .then(() => makeSocketAndConnect())
        .catch((err) => error('Starting up the server failed: ', err));
    }
    else {
      info("Connecting to: ", connectURL);
      makeSocketAndConnect();
    }
  // Otherwise, try starting up a server and waiting for it, then doing the work
  } else {
    info("No pidFile found, starting up server");
    startupServer(portFile)
      .then(() => makeSocketAndConnect())
      .catch((err) => console.error('Starting up the server failed: ', err));
  }

  process.on('SIGINT', function() {
    info("Caught interrupt signal, shutting down server");
    shutdown();
  });

}

module.exports = {
  start: start
}
