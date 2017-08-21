{
  provides: {},
  requires: [],
  nativeRequires: ['http', 'ws'],
  theModule: function(runtime, _, uri, http, ws) {

    const INFO = 4;
    const LOG = 3;
    const WARN = 2;
    const ERROR = 1;
    const SILENT = 0;
    var LOG_LEVEL = WARN;

    function makeLogger(level) {
      return function(...args) {
        if(LOG_LEVEL >= level) {
          info(console, ["[client] ", new Date()].concat(args));
        }
      }
    }

    const info = makeLogger(INFO);
    const log = makeLogger(LOG);
    const warn = makeLogger(WARN);
    const error = makeLogger(ERROR);


    // Port could be a string for a file path, like /tmp/some-sock,
    // or it could be a number, like 1705
    const makeServer = function(port, onmessage) {
      //info("Starting up server");
      return runtime.pauseStack(function(restarter) {
        var server = http.createServer(function(request, response) {
          response.writeHead(404);
          response.end();
        });
        server.listen(port, function() {
          info((new Date()) + ' Server is listening on port ' + port);
          info((new Date()) + ' The server\'s working directory is ' + process.cwd());
        });

        var wsServer = new ws.Server({
          server: server
        });

        // TODO(joe): Catalog what origins come from our clients.
        function originIsAllowed(origin) {
          return true;
        }

        wsServer.on('connection', function(connection) {
          
          info((new Date()) + ' Connection accepted.');

          function respond(jsonData) {
            info("Sending: ", jsonData);
            connection.send(jsonData);
            return runtime.nothing;
          }
          const respondForPy = runtime.makeFunction(respond, "respond");

          connection.on('message', function(message) {
            info('Received Message: ' + message);
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
                // info("Success: ", result);
              }
            });
          });
          connection.on('close', function(reasonCode, description) {
            // info((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
          });
        });
        
        info("Server startup successful");
        process.send({type: 'success'});

        process.on('SIGINT', function() {
          info("Caught interrupt signal, exiting server");
          restarter.resume(runtime.nothing)
        });
      });
    };

    return runtime.makeModuleReturn({
      "make-server": runtime.makeFunction(makeServer, "make-server")
    }, {});
  }
}
