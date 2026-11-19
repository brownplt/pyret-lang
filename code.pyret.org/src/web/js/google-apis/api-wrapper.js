// NOTE: Each of the following should be bound in the global scope:
//   - `gapi'     : Google API Javascript Client
//   - `apiKey'   : Google API Key
//   - `Q'        : Q Promise Framework

/**
 * The most recently authenticated version of the
 * wrapped Google API
 */
var gwrap = window.gwrap = {
  // Initialize to a dummy method which loads the wrapper
  load: function(params) {
    if (!params || !params.reauth || (params.reauth.immediate === undefined)) {
      throw new Error("Google API Wrapper not yet initialized");
    }
    var ret = Q.defer();
    loadAPIWrapper(params.reauth.immediate)
      .then(function(gw) {
        // Shallow copy
        var copy = $.extend({}, params);
        delete copy.reauth;
        // NOTE: loadAPI returns null (not a promise) when params.callback is
        // given -- that form delivers the API through the callback instead, and
        // its callers (drive.js, sheets.js) ignore this return value. Without
        // this guard the `.then` below threw a TypeError on every such load,
        // which Q swallowed into a rejection nobody observes. It also means
        // `hasAuth` is only ever attached in the promise form; callback-form
        // callers should ask hasGoogleAuth() instead.
        var loadedP = gw.load(copy);
        if (!loadedP) { return; }
        loadedP
          .then(function(loaded) {
            loaded.auth = gw.auth; // NOTE(joe): hmmm
            loaded.hasAuth = function() { return gw.auth !== null; }
            ret.resolve(loaded);
          });
      })
      .fail(function(err) {
        ret.reject(err); 
      });
    return ret.promise;
  },
  request: function() {
    throw new Error("Google API Wrapper not yet initialized");
  }
};

/**
 * Reauthenticates the current session.
 * @param {boolean} immediate - Whether the user needs to log in with
 *        Google (if false, the user will receive a refreshed access token).
 * @param {boolean} useFullScopes - Whether to include advanced scopes like
 *        spreadsheets and photos.
 * @returns A promise which will resolve following the re-authentication.
 */
// The outcome of the most recent immediate reauth: an auth object, or null
// when we hold no token. gwrap's own hasAuth() is attached only to the
// fully-built wrapper, which the callback form of load() never hands back, so
// this is the one signal available to every caller.
var _lastReauth = null;
window.hasGoogleAuth = function hasGoogleAuth() { return _lastReauth !== null; };

function reauth(immediate, useFullScopes) {
  var d = Q.defer();
  if (!immediate) {
    var path = "/login?redirect=" + encodeURIComponent("/close.html");
    if(useFullScopes) {
      path += "&scopes=full";
    }

    // Track whether we've already resolved to avoid double-resolution
    var resolved = false;
    function resolveOnce(method) {
      if (!resolved) {
        console.log("INFO: Popup login resolved by: ", method);
        resolved = true;
        // NOTE(joe): A useful thing to do for testing is to comment out this
        // cleanup(), and check which of the 3 methods are returning success
        // here. cleanup() will stop others from triggering.
        cleanup();
        d.resolve(reauth(true, useFullScopes));
      }
      else {
        console.log("INFO: Popup login resolved again (ignored): ", method);
      }
    }

    // Cleanup function to remove all listeners
    var channel = null;
    function cleanup() {
      window.removeEventListener('message', messageHandler);
      window.removeEventListener('storage', storageHandler);
      try { localStorage.removeItem('pyret_auth_complete'); } catch (err) {}
      if (channel) {
        try { channel.close(); }
        finally { channel = null; }
      }
    }

    // Method 1: Traditional postMessage (works when COOP allows window.opener)
    function messageHandler(e) {
      // e.domain appears to not be defined in Firefox
      if ((e.domain || e.origin) === document.location.origin) {
        resolveOnce("postMessage");
      }
    }
    window.addEventListener('message', messageHandler);

    // Method 2: BroadcastChannel (works even when COOP severs window.opener)
    // This is the fallback for environments like GoGuardian that inject COOP headers
    if (typeof BroadcastChannel !== 'undefined') {
      try {
        channel = new BroadcastChannel('pyret_auth');
        channel.onmessage = function(e) {
          if (e.data && e.data.type === 'auth_complete') {
            resolveOnce("Broadcast");
          }
        };
      } catch (e) {
        console.warn("BroadcastChannel setup failed:", e);
      }
    }

    // Method 3: localStorage fallback for very old browsers without BroadcastChannel
    function storageHandler(e) {
      if (e.key === 'pyret_auth_complete') {
        resolveOnce("localStorage");
        // Clean up the flag
        try { localStorage.removeItem('pyret_auth_complete'); } catch (err) {}
      }
    }
    // Clear any stale auth flag before opening popup
    try { localStorage.removeItem('pyret_auth_complete'); } catch (e) {}
    window.addEventListener('storage', storageHandler);

    // Need to do a login to get a cookie for this user; do it in a popup
    window.open(path);
  } else {
    // The user may be logged in, but needs an access token from our server.
    // /getAccessToken answers 200 either way and reports the outcome in
    // `logged_in` -- it deliberately does not use an error status for "not
    // logged in", so that the common logged-out case doesn't put a failed
    // request in the console. Resolving null (rather than rejecting) is the
    // established "no auth" signal to callers.
    var newToken = $.ajax("/getAccessToken", { method: "get", dataType: "json" });
    newToken.then(function(t) {
      if(!t || !t.logged_in) { _lastReauth = null; d.resolve(null); return; }
      gapi.auth.setToken({ access_token: t.access_token });
      logger.log('login', {user_id: t.user_id});
      _lastReauth = { user_id: t.user_id, access_token: t.access_token };
      d.resolve(_lastReauth);
    });
    // Reaching here now means a genuine transport/server failure, since the
    // not-logged-in answer arrives as a 200.
    newToken.fail(function(t) {
      _lastReauth = null;
      d.resolve(null);
    });
  }
  return d.promise;
}
window.reauth = reauth;

// GAPI Client APIs whose methods have been wrapped with calls to gQ
// (This is kept in the global state to avoid unneeded reloading)
var _GWRAP_APIS = {};

/**
 * Base module for interfacing with Google APIs.
 * Note that any APIs will only be authenticated
 * with the OAuth scopes found in /src/google-auth.js.
 * @param {boolean} immediate - See the description of `immediate`
 *        on `reauth`.
 */
function loadAPIWrapper(immediate) {

  // Sanity check: Make sure aforementioned things are
  //               actually defined.

  /**
   * Raises an initialization error if a variable with
   * the given name is not defined in the global scope
   * @param {string} v - The variable to check
   */
  function assertDefined(v) {
    if (window[v] === undefined) {
      throw new Error("Cannot initialize wrapper! " +
                      "Missing required definition: " + v);
    }
  }
  assertDefined('gapi');
  assertDefined('apiKey');
  assertDefined('Q');
  // Sanity check passed.

  // Custom error type definitions
  // http://stackoverflow.com/questions/1382107/whats-a-good-way-to-extend-error-in-javascript
  /**
   * Creates a new Google API Error
   * @constructor
   * @param [message] - The error message for this error
   */
  function GoogleAPIError(message) {
    Error.captureStackTrace(this, this.constructor);
    this.name = this.constructor.name;
    this.message = message || "";
  }

  GoogleAPIError.prototype = Object.create(Error.prototype);
  GoogleAPIError.prototype.name = "GoogleAPIError";
  GoogleAPIError.prototype.constructor = GoogleAPIError;

  /**
   * Alias for {@link GoogleAPIError}.
   * @see GoogleAPIError
   */
  var GAPIError = GoogleAPIError;

  /**
   * Creates a new Authentication Error. These are caused by
   * insufficient permissions.
   * @constructor
   * @see GoogleAPIError
   * @param [message] - The error message for this error
   */
  function AuthenticationError(message) {
    this.message = message || "";
  }
  AuthenticationError.prototype = Object.create(GoogleAPIError.prototype);
  AuthenticationError.prototype.name = "AuthenticationError";
  AuthenticationError.prototype.constructor = AuthenticationError;

  /**
   * Creates a new API Response Error. These are caused by invalid/error
   * responses from the Google API; e.g. 404 Not Found errors
   * @constructor
   * @see GoogleAPIError
   * @param [message] - The error message for this error
   */
  function APIResponseError(response) {
    response = response || {};
    this.message = response.message || "";
    this.code = response.code || null;
    this.response = response;
  }
  APIResponseError.prototype = Object.create(GoogleAPIError.prototype);
  APIResponseError.prototype.name = "APIResponseError";
  APIResponseError.prototype.constructor = APIResponseError;

  // Function definitions

  /**
   * Reauthenticates the current session with Google.
   * @see reauth
   * @param {boolean} immediate - DEPRECATED: UNUSED
   */
  function refresh(immediate) {
    if (arguments.length > 0) {
      console.warn("The `immediate` argument to `refresh()` is deprecated.");
    }
    return reauth(true);
  }

  /**
   * Runs the given thunk. If running it results in an
   * authentication failure, the session is re-authorized and the
   * function is run again.
   * @param {function} f - The thunk to run. It should return a promise.
   * @returns The given promise plus any needed re-authentication
   */
  function authCheck(f) {
    function isAuthFailure(result) {
      return result
        && ((result.error && result.error.code && result.error.code === 401)
            || (result.code && result.code === 401));
    }
    var retry = f().then(function(result) {
      if(isAuthFailure(result)) {
        return refresh().then(function(authResult) {
          if(!authResult || authResult.error) {
            return { error: { code: 401, message: "Couldn't re-authorize" } };
          } else {
            return f();
          }
        });
      } else {
        return result;
      }
    });
    return retry.then(function(result) {
      if(isAuthFailure(result)) {
        throw new AuthenticationError("Authentication failure");
      }
      return result;
    });
  }

  /**
   * Wraps the given promise with a check for a failed
   * API response.
   * @param {Promise} p - The promise to wrap
   * @returns {Promise} The wrapped promise
   */
  function failCheck(p) {
    return p.then(function(result) {
      // Network error
      if (result && (typeof result.code === "number") && (result.code >= 400)) {
        console.error("40X Error getting Google API response: ", result);
        throw new APIResponseError(result);
      }
      if (result && result.error) {
        console.error("Error getting Google API response: ", result);
        throw new APIResponseError(result);
      }
      return result;
    });
  }

  /**
   * Wraps the given Google query in a promise
   * @param request - The request object to wrap (made by methods from `gapi')
   * @param {boolean} [skipAuth] - If true, performs the request without
   *        authentication.
   * @returns {Promise} A promise which resolves to the result of the Google query
   */
  function gQ(makeRequest, skipAuth) {
    var oldAccess = gapi.auth.getToken();
    if (skipAuth) { gapi.auth.setToken({ access_token: null }); }
    var ret = failCheck(authCheck(function() {
      var d = Q.defer();
      // TODO: This should be migrated to a promise
      makeRequest().execute(function(result) {
        d.resolve(result);
      });
      return d.promise;
    }));
    if (skipAuth) {
      // NOTE(joe): see discussion at https://github.com/brownplt/code.pyret.org/issues/255
      // for why (A) we do this before the request completes and (B) the
      // setTimeout here is necessary
      setTimeout(function() {
        gapi.auth.setToken(oldAccess);
      });
    }
    return ret;
  }

  var cachedAPIS = [];

  /**
   * Loads the Google API denoted by the given parameters
   * @param {object} params - The designator for the API. Valid fields
            are `name`, `url`, `version`, and `callback`
   * @returns Nothing if `params.callback` is provided, otherwise a promise
   *          which resolves to the loaded API/APIs.
   */
  function loadAPI(params) {
    if (!params) {
      throw new GoogleAPIError("Missing API loading parameters");
    }
    if (params.reauth) {
      if (params.reauth.immediate === undefined) {
        throw new GoogleAPIError("Missing required field to load(): "
                                 + "params.reauth.immediate");
      }
      var reloaded = Q.defer();
      loadAPIWrapper(params.reauth.immediate)
        .then(function(gw) {
          // Shallow copy
          var copy = $.extend({}, params);
          delete copy.reauth;
          gw.load(copy)
            .then(function(loaded) {
              reloaded.resolve(loaded);
            });
        });
      return reloaded.promise;
    }
    for (var i = 0; i < cachedAPIS.length; ++i) {
      var cached = cachedAPIS[i];
      if (params.name && (cached.query.name === params.name)) {
        return cached.api;
      } else if (params.url && (cached.query.url === params.url)){
        return cached.api;
      }
    }
    var preKeys = Object.keys(gapi.client);

    function processKey(key) {
      function processObject(obj, dest) {
        Object.keys(obj).forEach(function(key) {
          if (typeof(obj[key]) === "object") {
            dest[key] = {};
            processObject(obj[key], dest[key]);
          } else if (typeof(obj[key]) === "function") {
            // Use one argument, since it seems that's all that's
            // used, and there's no `Function.prototype.apply` equivalent
            // that doesn't change the function's context
            var original = obj[key];
            dest[key] = (function(args, skipAuth) {
              return gQ(function() { return original(args); }, skipAuth);
            });
          } else {
            dest[key] = obj[key];
          }
        });
      }

      if (key in _GWRAP_APIS) {
        // Do nothing
        return _GWRAP_APIS[key];
      }
      var api = gapi.client[key];
      _GWRAP_APIS[key] = {};
      // NOTE(joe): hack as I'm testing on MS Edge
      if(api !== undefined) {
        processObject(api, _GWRAP_APIS[key]);
      }
      return _GWRAP_APIS[key];
    }

    function processDelta() {
      debugger;
      var newKeys = Object.keys(gapi.client)
            .filter(function(k) {return (preKeys.indexOf(k) === -1);});
      var ret;
      if (params.name && newKeys.indexOf(params.name) !== -1) {
        // When we requested a specific API by name, return only that one.
        // Other new keys (from concurrent loads) will be picked up by
        // their own loadAPI calls. Process all keys to populate the cache,
        // but only return the one we asked for.
        newKeys.forEach(processKey);
        ret = _GWRAP_APIS[params.name];
      } else if (newKeys.length > 1) {
        // NOTE(joe, Apr 2026): We added this to diagnose if it actually ever
        // happens in prod that we return a list of new APIs.
        // We think it probably doesn't based on call sites, but don't want to
        // break anything.
        logger.log('processdelta-multi-key-return', { newKeys, preKeys });
        ret = newKeys.map(processKey);
      } else if (params.name && newKeys.length === 0) {
        // Hack to make drive-loading happy on login
        if (gapi.client[params.name]) {
          ret = processKey(params.name);
        }
      } else {
        ret = processKey(newKeys[0]);
      }
      if (!ret) {
        console.warn("loadAPI: Nothing to return!");
      } else {
        cachedAPIS.push({query: params, api: ret});
      }
      return ret;
    }

    var name = params.name || params.url;
    var version = params.version;

    if (params.callback) {
      gapi.client.load(name, version, function() {
        params.callback(processDelta());
      });
      return null;
    } else {
      var ret = Q.defer();
      gapi.client.load(name, version, function() {
        ret.resolve(processDelta());
      });
      return ret.promise;
    }
  }

  var initialAuth = reauth(immediate);
  return initialAuth.then(function(auth) {
    /**
     * Creates the API Wrapping module to export
     */
    function makeWrapped() {
      this.GoogleAPIError = GoogleAPIError;
      this.GAPIError = GAPIError;
      this.AuthenticationError = AuthenticationError;
      this.APIResponseError = APIResponseError;
      this.load = loadAPI;
      this.withAuth = function(f) {return failCheck(authCheck(f));};
      this.request = (function(params, skipAuth) {
        return gQ(function() { return gapi.client.request(params); }, skipAuth);
      });
      this.auth = auth;
      this.hasAuth = function() { return auth !== null; };
    }

    makeWrapped.prototype = _GWRAP_APIS;

    gwrap = new makeWrapped();
    return gwrap;
  });

}
