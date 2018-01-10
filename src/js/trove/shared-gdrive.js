// Since we can't require a google login to package Pyret with an API key,
// we can't directly use the API to retrieve files.

({
  requires: [],
  nativeRequires: ['deasync', 'request'],
  provides: {},
  theModule: function(RUNTIME, NAMESPACE, uri, deasync, request) {
    var CPO_SHARED_FILE_BASE_URL = "https://code.pyret.org/shared-file?sharedProgramId="

    // This is a method for circumventing the standard google API
    var GDRIVE_DOWNLOAD_BASE_URL = "https://drive.google.com/uc?export=view&id="

    // The API calls have to be synchronous, because we can't mix the FFI and traditional JS callbacks. 
    // Note that in the `filelib` module, the file operations are synchronous as well.
    function syncReq(url, method, isJson) {
      var execReq = deasync(function(cb) {
        request({
          url: url,
          method: method,
          json: isJson,
          header: {
            // Todo: add version information
            'User-Agent': 'Pyret v0.0.0'
          },
        }, function (err, res, body) {

          /*console.error(err);
          console.error(body);
          console.error(res);*/

          if (err) {
            cb(err, null)
          } else if (res.statusCode != 200) {
            cb(new Error('Error: ' + body.toString()), null);
          }
        
          cb(null, body);
        });
      });

      return execReq();
    }

    var vals = {
      "get-shared-program": RUNTIME.makeFunction(function(filename, programId) {
          RUNTIME.ffi.checkArity(2, arguments, "get-shared-program", false);
          RUNTIME.checkString(filename);
          RUNTIME.checkString(programId);

          programInfo = syncReq(CPO_SHARED_FILE_BASE_URL + encodeURIComponent(programId), 'GET', true);

          if (programInfo['title'] != filename) {
            throw Error("Expected file with id " + programId + " to have name " + filename + ", but its name was " + programInfo['title'])
          }

          // Extract Google Drive file ID and download without the use of the Google Drive API
          selfLinkSegments = programInfo['selfLink'].split('/');
          gdriveFileID = selfLinkSegments.pop() || selfLinkSegments.pop();

          programSource = syncReq(GDRIVE_DOWNLOAD_BASE_URL + encodeURIComponent(gdriveFileID), 'GET', false);

          //console.log(GDRIVE_DOWNLOAD_BASE_URL + encodeURIComponent(gdriveFileID));
          //console.log(programInfo);
          //console.log(programSource);

          return RUNTIME.makeObject({
            src: programSource,
            modifiedDate: Number(new Date(programInfo['modifiedDate']))
          });

      }, "get-shared-program")
    }

    return RUNTIME.makeModuleReturn(vals, {});
  }

})