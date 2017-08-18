{
  provides: {},
  requires: [],
  nativeRequires: ['http', 'ws'],
  theModule: function(runtime, _, uri, http, ws) {

    // Port could be a string for a file path, like /tmp/some-sock,
    // or it could be a number, like 1705
    const makeServer = function(port, onmessage) {
      //console.log("Starting up server");
      return runtime.pauseStack(function(restarter) {
        var server = http.createServer(function(request, response) {
          response.writeHead(404);
          response.end();
        });
        server.listen(port, function() {
          console.log((new Date()) + ' Server is listening on port ' + port);
          console.log((new Date()) + ' The server\'s working directory is ' + process.cwd());
        });

        var wsServer = new ws.Server({
          server: server
        });

        // TODO(joe): Catalog what origins come from our clients.
        function originIsAllowed(origin) {
          return true;
        }

        wsServer.on('connection', function(connection) {
          
          console.log((new Date()) + ' Connection accepted.');

          function respond(jsonData) {
            console.log("Sending: ", jsonData);
            connection.send(jsonData);
            return runtime.nothing;
          }
          const respondForPy = runtime.makeFunction(respond, "respond");

          connection.on('message', function(message) {
            console.log('Received Message: ' + message);
            runtime.runThunk(function() {
              return onmessage.app(message, respondForPy);
            }, function(result) {
              if(runtime.isFailureResult(result)) {
                console.error("Failed: ", result.exn.exn, result.exn.stack, result.exn.pyretStack);
                connection.close();
                restarter.error(result.exn);
              }
              else {
                connection.close();
                // console.log("Success: ", result);
              }
            });
          });
          connection.on('close', function(reasonCode, description) {
            // console.log((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
          });
        });
        
        console.log("Server startup successful");
        process.send({type: 'success'});

        process.on('SIGINT', function() {
          console.log("Caught interrupt signal, exiting server");
          restarter.resume(runtime.nothing)
        });
      });
    };

    return runtime.makeModuleReturn({
      "make-server": runtime.makeFunction(makeServer, "make-server")
    }, {});
  }
}
