// Code from demo at https://www.npmjs.com/package/websocket
const WebSocketClient = require('websocket').client;
const lockFile = require('lockfile');
const childProcess = require('child_process');
const fs = require('fs');

function start(options) {

  const client = new WebSocketClient();

  const serverModule = options.client.compiler;

  const pidFile = ".pyret-parley." + options.client.port + ".pid";

  function shutdown() {
    try {
      const pid = Number(fs.readFileSync(pidFile));
      fs.unlinkSync(pidFile);
      process.kill(pid, 'SIGINT');
      console.log("Sent kill signal to " + pid);
    }
    catch(e) {
      console.log("No process to quit: " + e);
    }
  }

  if(options.client.shutdown) {
    shutdown();
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
    connection.sendUTF(JSON.stringify(options['pyret-options']));
  });

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

  if (lockFile.checkSync(".pyret-parley." + options.client.port + ".running.lock")) {
    // If the compiler is newer than the server process, restart
    var compilerStats = fs.lstatSync(serverModule);
    var pidStats = fs.lstatSync(pidFile);
    if(pidStats.mtime.getTime() < compilerStats.mtime.getTime()) {
      console.log("A running server was found, but the chosen compiler (" + serverModule + ") is newer than the server.  Restarting...");
      shutdown();
      startupServer(options.client.port)
        .then(() => client.connect('ws://localhost:' + options.client.port, 'parley'))
        .catch((err) => console.error('Starting up the server failed: ', err));
    }
    else {
      client.connect('ws://localhost:' + options.client.port, 'parley');
    }
  // Otherwise, try starting up a server and waiting for it, then doing the work
  } else {
    console.log("No lockfile found, starting up server");
    startupServer(options.client.port)
      .then(() => client.connect('ws://localhost:' + options.client.port, 'parley'))
      .catch((err) => console.error('Starting up the server failed: ', err));
  }

  process.on('SIGINT', function() {
    console.log("Caught interrupt signal, killing and restarting server");
    shutdown();
  });

}

module.exports = {
  start: start
}
