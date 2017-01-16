({
  requires: [],
  provides: {},
  nativeRequires: ["request"],
  theModule: function(RUNTIME, NAMESPACE, uri, http) {
    var gf = RUNTIME.getField;

    function execute(request) {
      RUNTIME.ffi.checkArity(1, arguments, "execute");
      RUNTIME.checkObject(request);

      var reqFields = {
        protocol: RUNTIME.unwrap(gf(request, "req-protocol")),
        port: RUNTIME.unwrap(gf(request, "req-port")),
        hostname: RUNTIME.unwrap(gf(request, "req-hostname")),
        path: RUNTIME.unwrap(gf(request, "req-path")),
        method: RUNTIME.unwrap(gf(request, "req-method"))
      };

      var options = {
        uri: reqFields.protocol + "//" + reqFields.hostname + reqFields.path,
        method: reqFields.method
      };

      RUNTIME.pauseStack(function(restarter) {
        http(options, function (error, response, body) {
          if (!error) {
            restarter.resume(RUNTIME.makeObject({
              'rsp-code': RUNTIME.makeNumber(response.statusCode),
              'rsp-headers': RUNTIME.makeObject(response.headers),
              'rsp-body': RUNTIME.makeString(body)
            }));
          } else {
            console.error(error);
            RUNTIME.raise(RUNTIME.makeString('Unknown error in httplib'));
          }
        });
      });
    }

    return RUNTIME.makeModuleReturn({
      'execute': RUNTIME.makeFunction(execute, "execute")
    }, {});
  }
})
