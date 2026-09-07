window.logger = {};

var ConsoleBackend = function() {
  this.log = function() {
    console.log.apply(console, arguments);
  };
};

var DummyBackend = function () {
  this.log = function(_, __){};
};

var FetchWithCredsBackend = function (url, user, pass) {
  const auth = btoa(user + ":" + pass);
  this.log = function(name, obj) {
		const opts = {
				method: 'POST',
				credentials: "include",
				headers: {
						"Content-Type": "application/json",
						'Authorization': 'Basic ' + auth
				},
				body: JSON.stringify(obj)
		};
		const req = fetch("https://bootstrapworld.org/data/actions/LogActions.php?method=pyretLog", opts).then((resp) => {
			console.log(resp);
		}).catch((e) => console.error(e));
  };

}

var AJAXBackend = function (url) {
  this.log = function (name, obj) {
    var request = new XMLHttpRequest();
    request.open("POST", url, true);
    request.send(JSON.stringify(obj));
  }
}

var backend;
if(window.LOG_URL && window.LOG_USER) {
	backend = new FetchWithCredsBackend(LOG_URL, LOG_USER, LOG_PASSWORD);
}
else if(window.LOG_URL) {
	backend = new AJAXBackend(LOG_URL);
}
else {
	backend = new DummyBackend();
}

var logger = (function(backend) {
  var sessionStorage;
  var localStorage;
  try {
    sessionStorage = window.localStorage;
    localStorage = window.sessionStorage;
  }
  catch(e) {
    sessionStorage = {
      getItem: function() { },
      setItem: function() { },
    };
    localStorage = {
      getItem: function() { },
      setItem: function() { },
    };
  }
  function guid() {
    var array = new Uint32Array(6);
    window.crypto.getRandomValues(array);
    var str = "";
    for(var i = 0; i < array.length; i++) {
      str = str.concat(array[i].toString(36));
    }
    return str;
  }

  /* Tab, Session, and Browser Identifiers */
  var identifiers = (function(){
    var _identifiers = {};
    
    function storedID(storage, type) {
      var id = storage.getItem(type);
      if (id === null || id === "") {
        id = _identifiers[type] || guid();
        _identifiers[type] = id;
        storage.setItem(type, id);
      }
      return id;
    }
    
    return {
      get localID () {
        return _identifiers['lid'] || storedID(localStorage, 'lid');
      },
      get sessionID () {
        return _identifiers['sid'] || storedID(sessionStorage, 'sid');
      },
      get windowID () {
        _identifiers['tid'] = _identifiers['tid'] || guid();
        return _identifiers['tid'];
      }
    };
  })();
  
  function log(name, obj) {
    if(!(obj instanceof Object))
      obj = {};
    // Which compiler flavor this page is running (see editor.html: resolved
    // before any script here runs), on EVERY record so ts-vs-stock counts
    // and rates (loads, runs, errors, load failures) can be compared
    // directly. Pages with no flavor branch (blocks) report the stock name.
    obj.compiler = window.CPO_COMPILER || "pyret";
    obj.meta = [  Date.now()
                , name
                , identifiers.windowID
                , identifiers.sessionID
                , identifiers.localID
                , GIT_REV
                , GIT_BRANCH];
    backend.log(name, obj);
  }
  
  // Set window.LOG_DETAILED = true in a forked deploy (e.g. a study) to
  // enable per-edit logging in beforePyret/beforeBlocks and the CodeMirror
  // change/focus/blur hooks below.
  return {
    guid  : guid,
    log   : log,
    isDetailed: window.LOG_DETAILED === true
  };
})(backend);

if(window.CodeMirror) {
  CodeMirror.defineOption('logging', false, 
    function (cm, new_value) {
      if (new_value != true)
        return;
      if(!cm.CPO_editorID)
        cm.CPO_editorID = logger.guid();
      logger.log('cm_init', {CPO_editorID: cm.CPO_editorID});
      cm.on("change", function(cm, change) {
        if(logger.isDetailed) {
          change.CPO_editorID = cm.CPO_editorID;
          logger.log('cm_change', change);
        }
      });
      cm.on("focus", function(cm) {
        if(logger.isDetailed) {
          logger.log('cm_focus', {CPO_editorID: cm.CPO_editorID});
        }
      });
      cm.on("blur", function(cm) {
        if(logger.isDetailed) {
          logger.log('cm_blur',  {CPO_editorID: cm.CPO_editorID});
        }
      });
    });
}

// Log the loading of the logger (near the begining of page load)  
logger.log('load');

// Log page unload
window.addEventListener('unload', function(event) {
  logger.log('unload');
});

// Log tab/window visibility change (kinda unreliable)
document.addEventListener("visibilitychange", function() {
  logger.log('pagevis', {visibility: document.visibilityState});
});
