{
  provides: {
    values: {
      "make-server": "tany"
    }
  },
  requires: [],
  nativeRequires: ['http', 'ws'],
  theModule: function(runtime, _, uri, http, ws) {

    const INFO = 4;
    const LOG = 3;
    const WARN = 2;
    const ERROR = 1;
    const SILENT = 0;
    var LOG_LEVEL = ERROR;

    function makeLogger(level) {
      return function(...args) {
        if(LOG_LEVEL >= level) {
          console.log.apply(console, ["[server] ", new Date()].concat(args));
        }
      }
    }

    const info = makeLogger(INFO);
    const log = makeLogger(LOG);
    const warn = makeLogger(WARN);
    const error = makeLogger(ERROR);


    // Port is a string for a file path, like /tmp/some-sock,
    const makeServer = function(port, onmessage) {

      var runQueue = [];

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

        // At this point, using port as a file socket didn't fail, so make sure
        // to remove it when we shut down.
        process.on('SIGINT', function() {
          if(fs.existsSync(port)) {
            fs.unlinkSync(port);
          }
        });
        process.on('exit', function() {
          if(fs.existsSync(port)) {
            fs.unlinkSync(port);
          }
        });

        var wsServer = new ws.Server({
          server: server
        });

        wsServer.on('connection', function(connection) {

          function respond(jsonData) {
            info("Sending: ", jsonData);
            connection.send(jsonData);
            return runtime.nothing;
          }
          function respondJSON(json) { return respond(JSON.stringify(json)); }
          const respondForPy = runtime.makeFunction(respond, "respond");

          function tryQueue() {
            info("Trying run queue, length is ", runQueue.length);
            if(runQueue.length > 0) {
              var current = runQueue.pop();
              runtime.runThunk(function() {
                return onmessage.app(current, respondForPy);
              }, function(result) {
                if(runtime.isFailureResult(result)) {
                  error("Failed: ", result.exn.exn, result.exn.stack, result.exn.pyretStack);
                  respondJSON({type: "echo-err", contents: "There was an internal error, please report this as a bug"});
                  respondJSON({type: "echo-err", contents: String(result.exn.exn) });
                  connection.close();
                  // restarter.error(result.exn);
                }
                else {
                  connection.close();
                  // info("Success: ", result);
                }
                tryQueue();
              });
            }
          }

          
          info((new Date()) + ' Connection accepted.');


          connection.on('message', function(message) {
            info('Received Message: ' + message);

            var parsed = JSON.parse(message);

            if(parsed.command === "stop") {
              runtime.schedulePause(function(restarter) {
                restarter.break(); 
              });
              tryQueue();
            }

            if(parsed.command === "shutdown") {
              runtime.breakAll();
              info("Exiting due to shutdown request");
              process.exit(0);
            }

            if(parsed.command === "compile") {
              runQueue.push(parsed.compileOptions);
              tryQueue();
            }

          });
          connection.on('close', function(reasonCode, description) {
            // info((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
          });
        });
        
        info("Server startup successful");
        if(process.send) {
          process.send({type: 'success'});
        }

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
