// Code from demo at https://www.npmjs.com/package/websocket
const WebSocketClient = require('websocket').client;
const commandLineArgs = require('command-line-args')
 
const client = new WebSocketClient();
 
const optionDefinitions = [
  { name: 'program', alias: 'p', type: String },
  { name: 'outfile', alias: 'o', type: String },
  { name: 'require-config', alias: 'r', type: String },
]

client.on('connectFailed', function(error) {
  console.log('Connect Error: ' + error.toString());
});
 
client.on('connect', function(connection) {
  console.log('WebSocket Client Connected');
  connection.on('error', function(error) {
    console.log("Connection Error: " + error.toString());
  });
  connection.on('close', function() {
    console.log('parley closed');
  });
  connection.on('message', function(message) {
    if (message.type === 'utf8') {
        console.log("Received: '" + message.utf8Data + "'");
    }
  });

  connection.sendUTF(JSON.stringify(commandLineArgs(optionDefinitions)));
  connection.close();
});
 
client.connect('ws://localhost:8080/', 'parley');
