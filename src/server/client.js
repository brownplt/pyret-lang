// Code from demo at https://www.npmjs.com/package/websocket
const WebSocketClient = require('websocket').client;
const commandLineArgs = require('command-line-args');
const lockFile = require('lockfile');
const childProcess = require('child_process');
const fs = require('fs');
 
const client = new WebSocketClient();

 
const optionDefinitions = [
  // These options affect how the client starts up and communiates with the server
  { name: 'shutdown', alias: 's', type: Boolean, group: "client" },
  { name: 'port', alias: 't', type: String, defaultValue: "1701", group: "client" },
  { name: 'compiler', alias: 'c', type: String, defaultValue: "build/phaseA/pyret.jarr", group: "client" },

  // These options are passed on to the compiler, and have no effect (yet)
  // on the client
  { name: 'program', type: String, group: "pyret-options" },
  { name: 'outfile', type: String, group: "pyret-options" },
  { name: 'require-config', type: String, group: "pyret-options" },
  { name: 'builtin-js-dir', type: String, group: "pyret-options" },
  { name: 'builtin-arr-dir', type: String, group: "pyret-options" },
  { name: 'check-mode', type: Boolean, group: "pyret-options", defaultValue: true },
  { name: 'compiled-dir', type: String, group: "pyret-options" }

];

const options = commandLineArgs(optionDefinitions).client;

const serverModule = options.compiler;

const pidFile = ".pyret-parley." + options.port + ".pid";

if(options.shutdown) {
  try {
    const pid = Number(fs.readFileSync(pidFile));
    fs.unlinkSync(pidFile);
    process.kill(pid, 'SIGINT');
  }
  catch(e) {
    console.error("Could not kill the process because the pid file " + pidFile + " didn't exist or was inaccessible: ", e);
  }
  process.exit(0);
}

client.on('connectFailed', function(error) {
  console.log('Connect Error: ' + error.toString());
});
 
client.on('connect', function(connection) {
  console.log('parley protocol connected');
  connection.on('error', function(error) {
    console.log("Connection Error: " + error.toString());
  });
  connection.on('close', function() {
    console.log('parley closed');
  });
  connection.on('message', function(message) {
    const parsed = JSON.parse(message.utf8Data);
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
  console.log("options: ", commandLineArgs(optionDefinitions)['pyret-options']);
  connection.sendUTF(JSON.stringify(commandLineArgs(optionDefinitions)['pyret-options']));
});

function startupServer(port) {
  const child = childProcess.fork(
    serverModule,
    ["-serve", "--port", port],
    { stdio: [0, 1, 2, 'ipc'] } // To send messages on completion of startup
  );

  return new Promise((resolve, reject) => {
    child.on('message', function(msg) {
      console.log("Message received from child: ", msg);
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

// If a server is starting up, fail and tell the client to wait
if(lockFile.checkSync(".pyret-parley." + options.port + ".startup.lock")) {
  console.error("The server is starting up in another process, and cannot be reached yet.  Retry this command in a few seconds.");
// If a server is already running, just connect to it and do the work
} else if (lockFile.checkSync(".pyret-parley." + options.port + ".running.lock")) {
  client.connect('ws://localhost:' + options.port, 'parley');
// Otherwise, try starting up a server and waiting for it, then doing the work
} else {
  console.log("No lockfile found, starting up server");
  startupServer(options.port)
    .then(() => client.connect('ws://localhost:' + options.port, 'parley'))
    .catch((err) => console.error('Starting up the server failed: ', err));
}

