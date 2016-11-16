{
  provides: {
    values: {
      "make-server": "tany"
    },
    aliases: {},
    datatypes: {}
  },
  requires: [],
  nativeRequires: ['http', 'websocket', 'lockfile'],
  theModule: function(runtime, _, uri, http, ws, lockFile) {

    // Code from demo at https://www.npmjs.com/package/websocket
    const makeServer = function(port, onmessage) {
      const lockname = ".pyret-parley." + port + ".running.lock";
      //console.log("Starting up server");
      runtime.pauseStack(function(restarter) {
        lockFile.lock(lockname, function(er) {
          if(er) {
            console.error("Could not acquire lock");
            process.send({type: 'fail', error: er});
            return runtime.nothing;
          }
          var server = http.createServer(function(request, response) {
            response.writeHead(404);
            response.end();
          });
          server.listen(port, function() {
            console.log((new Date()) + ' Server is listening on port ' + port);
          });

          var wsServer = new ws.server({
            httpServer: server,
          });

          // TODO(joe): Catalog what origins come from our clients.
          function originIsAllowed(origin) {
            return true;
          }

          wsServer.on('request', function(request) {
            if (!originIsAllowed(request.origin)) {
              // Make sure we only accept requests from an allowed origin 
              request.reject();
              // console.log((new Date()) + ' Connection from origin ' + request.origin + ' rejected.');
              return;
            }
            
            var connection = request.accept('parley', request.origin);

            // console.log((new Date()) + ' Connection accepted.');

            function respond(jsonData) {
              // console.log("Sending: ", jsonData);
              connection.sendUTF(jsonData);
              return runtime.nothing;
            }
            const respondForPy = runtime.makeFunction(respond, "respond");

            connection.on('message', function(message) {
              if (message.type === 'utf8') {
                // console.log('Received Message: ' + message.utf8Data);
                runtime.runThunk(function() {
                  onmessage.app(message.utf8Data, respondForPy);
                }, function(result) {
                  if(runtime.isFailureResult(result)) {
                    connection.close();
                    console.error("Failed: ", result.exn.exn, result.exn.stack, result.exn.pyretStack);
                    restarter.error(result.exn);
                  }
                  else {
                    connection.close();
                    // console.log("Success: ", result);
                  }
                });
              }
            });
            connection.on('close', function(reasonCode, description) {
              // console.log((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
            });
          });
          
          console.log("Server startup successful");
          process.send({type: 'success'});

          process.on('SIGINT', function() {
            console.log("Caught interrupt signal, exiting server");
            lockFile.unlockSync(lockname);
            restarter.resume(runtime.nothing)
          });
        });
      });
    };

    return runtime.makeModuleReturn({
      "make-server": runtime.makeFunction(makeServer, "make-server")
    }, {});
  }
}
