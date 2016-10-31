{
  provides: {},
  requires: [],
  nativeRequires: ['http', 'websocket'],
  theModule: function(runtime, _, uri, http, ws) {

    // Code from demo at https://www.npmjs.com/package/websocket
    var makeServer = function(port, onmessage) {
      var server = http.createServer(function(request, response) {
        console.log((new Date()) + ' Received request for ' + request.url);
        response.writeHead(404);
        response.end();
      });
      server.listen(8080, function() {
        console.log((new Date()) + ' Server is listening on port 8080');
      });

      var wsServer = new ws.server({
        httpServer: server,
      });

      // TODO(joe): Catalog what origins come from our clients.
      function originIsAllowed(origin) {
        console.log("Origin is: ", origin);
        return true;
      }

      wsServer.on('request', function(request) {
        if (!originIsAllowed(request.origin)) {
          // Make sure we only accept requests from an allowed origin 
          request.reject();
          console.log((new Date()) + ' Connection from origin ' + request.origin + ' rejected.');
          return;
        }
        
        var connection = request.accept('parley', request.origin);

        console.log((new Date()) + ' Connection accepted.');

        connection.on('message', function(message) {
          if (message.type === 'utf8') {
            console.log('Received Message: ' + message.utf8Data);
            runtime.runThunk(function() {
              onmessage.app(message.utf8Data);
            }, function(result) {
              if(runtime.isFailureResult(result)) {
                console.error("Failed: ", result.exn.exn, result.exn.stack, result.exn.pyretStack);
              }
              else {
                console.log("Success: ", result);
              }
            });
          }
        });
        connection.on('close', function(reasonCode, description) {
          console.log((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
        });
      });

      runtime.pauseStack(function(restarter) {
        // For now, do nothing!  Just leave the server running
      });
    };

    return runtime.makeModuleReturn({
      "make-server": runtime.makeFunction(makeServer, "make-server")
    }, {});
  }
}
