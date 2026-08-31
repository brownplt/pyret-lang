/* global $ jQuery CPO CodeMirror storageAPI Q createProgramCollectionAPI makeShareAPI */

var originalPageLoad = Date.now();
console.log("originalPageLoad: ", originalPageLoad);

// Transparently route browser fetches to allowlisted hosts through the
// server-side proxy at /load-shareurl, but only when the direct path doesn't
// work.
//
// Strategy: the FIRST fetch to an allowlisted host fires direct + proxied in
// parallel. We decide shouldProxy for the rest of the page-load from direct's
// response *headers*:
//   - direct returned 2xx with content-type text/plain  -> shouldProxy=false:
//     serve direct's response, abort the in-flight proxy fetch.
//   - direct failed, hung past timeout, or returned anything else
//                                                        -> shouldProxy=true:
//     serve proxy's response.
// A key idea is that network-blocky things sometimes return 200 with a
// message page about blocking (or an error, but that counts as a fail). We
// don't want to accidentally think that's a success.
// shouldProxy state is in-memory and per-host — never persisted, since
// reachability changes between networks and a stale value would silently
// break loads.
//
// Installed on the global fetch as early as possible so it catches every fetch
// caller; some of them are in the pyret-lang runtime and would be otherwise
// difficult to configure.
const SHAREURL_PROXY_HOSTS = new Set(['raw.githubusercontent.com']);
const SHAREURL_DIRECT_TIMEOUT_MS = 5000;
const _origFetch = window.fetch.bind(window);

const _shareurlShouldProxy = new Map();          // host -> boolean
const _shareurlShouldProxyInflight = new Map();  // host -> Promise<boolean>

function _shareurlProxyUrl(fetchInput) {
  return '/load-shareurl?url=' + encodeURIComponent(_shareurlInputToUrl(fetchInput));
}

function _shareurlInputToUrl(fetchInput) {
  return (typeof fetchInput === 'string') ? fetchInput
         : (typeof Request !== 'undefined' && fetchInput instanceof Request) ? fetchInput.url
         : String(fetchInput);
}

function _shareurlVerifyDirect(r) {
  if (!r.ok) return false;
  const ct = (r.headers.get('content-type') || '').toLowerCase();
  // Source files served from raw.githubusercontent.com come back as
  // text/plain (.arr, .json, .csv, .md all do). Anything else — HTML block
  // pages, captive portals, surprise content types — we don't trust as a
  // real upstream response.
  return ct.startsWith('text/plain');
}

function _shareurlFetch(shouldProxy, fetchInput, fetchInit) {
  const maybeProxyInput = shouldProxy ? _shareurlProxyUrl(fetchInput) : fetchInput;
  return _origFetch(maybeProxyInput, fetchInit);
}

function _shareurlRace(fetchInput, fetchInit) {
  const proxyCtrl = new AbortController();
  // NOTE(joe): The signal overwrite is technically not the right fetch()
  // polyfill. If the caller elsewhere in the codebase provided a different
  // signal (which in the fetch API is only for aborting as of April '26), that
  // caller aborting through that signal won't cancel the proxy fetch.
  // I'm OK letting that case slip through here in exchange for not having a
  // bunch of extra event handler forwarding
  const proxyP = _origFetch(_shareurlProxyUrl(fetchInput),
    Object.assign({}, fetchInit, { signal: proxyCtrl.signal }));
  const directP = _origFetch(fetchInput, fetchInit).then(r => {
    if (!_shareurlVerifyDirect(r)) throw new Error('direct request failed');
    return r;
  });

  // shouldProxy: false iff direct verified before the timeout, else true.
  // Whether to proxy is decided solely on whether direct succeeds or not
  const shouldProxyPromise = Promise.race([
    directP.then(() => false, () => true),
    new Promise(resolve => setTimeout(() => resolve(true), SHAREURL_DIRECT_TIMEOUT_MS)),
  ]);

  // Settlement-order check: if direct verifies before proxy returns, abort
  // the in-flight proxy to stop wasting server bandwidth. We must NOT
  // abort once proxy has already returned, since by then the caller is
  // reading proxy's body and aborting would error its stream mid-read.
  const directFinishedSuccessfullyAndFirstP = Promise.race([
    directP.then(() => true, () => false),
    proxyP.then(() => false, () => false),
  ]);
  directFinishedSuccessfullyAndFirstP.then(directFirst => {
    if (directFirst) proxyCtrl.abort();
  });

  // Caller's response: whichever of direct-verified or proxy-OK fulfills
  // first. A non-ok proxy response must NOT win while direct is still
  // pending: fetch fulfills on HTTP errors, and on hosts with no proxy
  // endpoint at all (static serving: the vscode webview, embed-static) the
  // local 404 arrives long before the real cross-origin response, which
  // would hand the caller a bogus 404. If BOTH fail, surface proxy's
  // response/error (the more authoritative upstream — direct's may just be
  // 'direct-not-verified').
  const responsePromise = Promise.any([
    directP,
    proxyP.then(r => {
      if (!r.ok) { const e = new Error('proxy response not ok'); e._shareurlResponse = r; throw e; }
      return r;
    }),
  ]).catch(aggErr => {
    const proxyErr = aggErr.errors[1];
    if (proxyErr && proxyErr._shareurlResponse) return proxyErr._shareurlResponse;
    return Promise.reject(proxyErr || aggErr.errors[0]);
  });

  return { responsePromise, shouldProxyPromise };
}

window.fetch = function(fetchInput, fetchInit) {
  let host;
  try { host = new URL(_shareurlInputToUrl(fetchInput), window.location.href).hostname; }
  catch (_) { return _origFetch(fetchInput, fetchInit); }
  if (!SHAREURL_PROXY_HOSTS.has(host)) return _origFetch(fetchInput, fetchInit);

  const shouldProxy = _shareurlShouldProxy.get(host);
  const inflight = _shareurlShouldProxyInflight.get(host);
  if (shouldProxy !== undefined) {
    return _shareurlFetch(shouldProxy, fetchInput, fetchInit);
  } else if (inflight) {
    // shouldProxy pending: queue this fetch on it and issue a single fresh
    // request once shouldProxy is decided.
    return inflight.then(sp => _shareurlFetch(sp, fetchInput, fetchInit));
  } else {
    // First fetch to this host this page-load: run the race.
    const { responsePromise, shouldProxyPromise } = _shareurlRace(fetchInput, fetchInit);
    _shareurlShouldProxyInflight.set(host, shouldProxyPromise);
    shouldProxyPromise.then(sp => {
      _shareurlShouldProxy.set(host, sp);
      _shareurlShouldProxyInflight.delete(host);
    });
    return responsePromise;
  }
};

const isEmbedded = window.parent !== window;

var shareAPI = makeShareAPI(process.env.CURRENT_PYRET_RELEASE);

var url = window.url = require('url.js');
var modalPrompt = require('./modal-prompt.js');
window.modalPrompt = modalPrompt;

const LOG = true;
window.ct_log = function(/* varargs */) {
  if (window.console && LOG) {
    console.log.apply(console, arguments);
  }
};

window.ct_error = function(/* varargs */) {
  if (window.console && LOG) {
    console.error.apply(console, arguments);
  }
};
var initialParams = url.parse(document.location.href);
var params = url.parse("/?" + initialParams["hash"]);
// Who owns this editor's initial contents? A standalone page installs its own
// (programLoaded below). An embedded instance (the embed API's iframe, the
// vscode webview) or a page booted from an initialState hash is host-fed: its
// real contents arrive via the events.js `reset` protocol, and boot isn't
// over until that reset fully settles -- reset() runs a warm-start program
// before installing contents, and driving the editor during that window races
// the host's own handshake. EDITOR_CONTENTS_SETTLED is the single "initial
// contents are in and the editor is quiescent" fact, declared at whichever of
// those two settle points applies (here for standalone; events.js reset() for
// host-fed), so observers don't have to re-derive per-host boot behavior.
window.EXPECTS_HOST_RESET = isEmbedded || !!params["get"]["initialState"];
window.highlightMode = "mcmh"; // what is this for?
window.clearFlash = function() {
  $(".notificationArea").empty();
}
window.whiteToBlackNotification = function() {
  /*
  $(".notificationArea .active").css("background-color", "white");
  $(".notificationArea .active").animate({backgroundColor: "#111111" }, 1000);
  */
};
window.stickError = function(message, more) {
  CPO.sayAndForget(message);
  clearFlash();
  var err = $("<span>").addClass("error").text(message);
  if(more) {
    err.attr("title", more);
  }
  err.tooltip();
  $(".notificationArea").prepend(err);
  whiteToBlackNotification();
};
window.flashError = function(message) {
  CPO.sayAndForget(message);
  clearFlash();
  var err = $("<span>").addClass("error").text(message);
  $(".notificationArea").prepend(err);
  whiteToBlackNotification();
  err.fadeOut(7000);
};
window.flashMessage = function(message) {
  CPO.sayAndForget(message);
  clearFlash();
  var msg = $("<span>").addClass("active").text(message);
  $(".notificationArea").prepend(msg);
  whiteToBlackNotification();
  msg.fadeOut(7000);
};
window.stickMessage = function(message) {
  CPO.sayAndForget(message);
  clearFlash();
  var msg = $("<span>").addClass("active").text(message);
  $(".notificationArea").prepend(msg);
  whiteToBlackNotification();
};
window.stickRichMessage = function(content) {
  CPO.sayAndForget(content.text());
  clearFlash();
  $(".notificationArea").prepend($("<span>").addClass("active").append(content));
  whiteToBlackNotification();
};
window.mkWarningUpper = function(){return $("<div class='warning-upper'>");}
window.mkWarningLower = function(){return $("<div class='warning-lower'>");}

var Documents = function() {

  function Documents() {
    this.documents = new Map();
  }

  Documents.prototype.has = function (name) {
    return this.documents.has(name);
  };

  Documents.prototype.get = function (name) {
    return this.documents.get(name);
  };

  Documents.prototype.set = function (name, doc) {
    if(logger.isDetailed)
      logger.log("doc.set", {name: name, value: doc.getValue()});
    return this.documents.set(name, doc);
  };

  Documents.prototype.delete = function (name) {
    if(logger.isDetailed)
      logger.log("doc.del", {name: name});
    return this.documents.delete(name);
  };

  Documents.prototype.forEach = function (f) {
    return this.documents.forEach(f);
  };

  return Documents;
}();

var VERSION_CHECK_INTERVAL = 120000 + (30000 * Math.random());

function checkVersion() {
  $.get("/current-version").then(function(resp) {
    resp = JSON.parse(resp);
    if(resp.version && resp.version !== process.env.CURRENT_PYRET_RELEASE) {
      window.flashMessage("A new version of Pyret is available. Save and reload the page to get the newest version.");
    }
  });
}
if(!isEmbedded) {
  window.setInterval(checkVersion, VERSION_CHECK_INTERVAL);
}

window.CPO = {
  save: function() {},
  autoSave: function() {},
  documents : new Documents()
};
$(function() {
  const CONTEXT_FOR_NEW_FILES = "use context starter2024\n";
  const CONTEXT_PREFIX = /^use context\s+/;

  function merge(obj, extension) {
    var newobj = {};
    Object.keys(obj).forEach(function(k) {
      newobj[k] = obj[k];
    });
    Object.keys(extension).forEach(function(k) {
      newobj[k] = extension[k];
    });
    return newobj;
  }
  var animationDiv = null;
  function closeAnimationIfOpen() {
    if(animationDiv) {
      animationDiv.empty();
      animationDiv.dialog("destroy");
      animationDiv = null;
    }
  }
  let activeEditor = null;
  CPO.makeEditor = function(container, options) {
    var initial = "";
    if (options.hasOwnProperty("initial")) {
      initial = options.initial;
    }

    var textarea = jQuery("<textarea aria-hidden='true'>");
    textarea.val(initial);
    container.append(textarea);

    var runFun = function (code, replOptions) {
      options.run(code, {cm: CM}, replOptions);
    };

    var useLineNumbers = !options.simpleEditor;
    var useFolding = !options.simpleEditor;

    var gutters = !options.simpleEditor ?
      ["help-gutter", "CodeMirror-linenumbers", "CodeMirror-foldgutter"] :
      [];

    function reindentAllLines(cm) {
      var last = cm.lineCount();
      cm.operation(function() {
        for (var i = 0; i < last; ++i) cm.indentLine(i);
      });
    }

    var CODE_LINE_WIDTH = 100;

    var rulers, rulersMinCol;

    // place a vertical line in code editor, and not repl
    if (options.simpleEditor) {
      rulers = [];
    } else{
      rulers = [{color: "#317BCF", column: CODE_LINE_WIDTH, lineStyle: "dashed", className: "hidden"}];
      rulersMinCol = CODE_LINE_WIDTH;
    }

    const mac = CodeMirror.keyMap.default === CodeMirror.keyMap.macDefault;
    console.log("Using keymap: ", CodeMirror.keyMap.default, "macDefault: ", CodeMirror.keyMap.macDefault, "mac: ", mac);
    const modifier = mac ? "Cmd" : "Ctrl";

    const extraKeys = {
        "Shift-Enter": function(cm) { runFun(cm.getValue()); },
        "Shift-Ctrl-Enter": function(cm) { runFun(cm.getValue()); },
        "Tab": "indentAuto",
        "Ctrl-I": reindentAllLines,
        "Esc Left": "goBackwardSexp",
        "Alt-Left": "goBackwardSexp",
        "Esc Right": "goForwardSexp",
        "Alt-Right": "goForwardSexp",
        "Ctrl-Left": "goBackwardToken",
        "Ctrl-Right": "goForwardToken",
        [`${modifier}-F`]: "findPersistent",
        [`${modifier}-/`]: "toggleComment",
      };
    if(window.PYRET_IN_VSCODE) {
      // Disable undo and redo in vscode, since they mess with the host editor's undo/redo stack
      // Oddly, it doesn't seem to work to add these to extraKeys; I have to
      // override them in the default keymap
      CodeMirror.keyMap.default[`${modifier}-Z`] = false;
      CodeMirror.keyMap.default[`Shift-${modifier}-Z`] = false;
      CodeMirror.keyMap.default[`${modifier}-Y`] = false;
      // Ctrl-U is Undo within a range
      CodeMirror.keyMap.default[`${modifier}-U`] = false;
    }

    var cmOptions = {
      keyMap: 'default',
      extraKeys: CodeMirror.normalizeKeyMap(extraKeys),
      indentUnit: 2,
      tabSize: 2,
      viewportMargin: Infinity,
      lineNumbers: useLineNumbers,
      matchKeywords: true,
      matchBrackets: true,
      styleSelectedText: true,
      foldGutter: useFolding,
      gutters: gutters,
      lineWrapping: true,
      logging: true,
      rulers: rulers,
      rulersMinCol: rulersMinCol,
      scrollPastEnd: true,
    };

    cmOptions = merge(cmOptions, options.cmOptions || {});

    var CM = CodeMirror.fromTextArea(textarea[0], cmOptions);
    CM.on("focus", () => {
      activeEditor = CM;
    });

    function firstLineIsNamespace() {
      const firstline = CM.getLine(0);
      const match = firstline.match(CONTEXT_PREFIX);
      return match !== null;
    }

    let namespacemark = null;
    function setContextLine(newContextLine) {
      var hasNamespace = firstLineIsNamespace();
      if(!hasNamespace && namespacemark !== null) {
        namespacemark.clear();
      }
      if(!hasNamespace) {
        CM.replaceRange(newContextLine, { line:0, ch: 0}, {line: 0, ch: 0});
      }
      else {
        CM.replaceRange(newContextLine, { line:0, ch: 0}, {line: 1, ch: 0});
      }
    }

    if(!options.simpleEditor) {

      const gutterQuestionWrapper = document.createElement("div");
      gutterQuestionWrapper.className = "gutter-question-wrapper";
      const gutterTooltip = document.createElement("span");
      gutterTooltip.className = "gutter-question-tooltip";
      gutterTooltip.innerText = "The use context line tells Pyret to load tools for a specific class context. It can be changed through the main Pyret menu. Most of the time you won't need to change this at all.";
      const gutterQuestion = document.createElement("img");
      gutterQuestion.src = window.APP_BASE_URL + "/img/question.png";
      gutterQuestion.className = "gutter-question";
      gutterQuestionWrapper.appendChild(gutterQuestion);
      gutterQuestionWrapper.appendChild(gutterTooltip);
      CM.setGutterMarker(0, "help-gutter", gutterQuestionWrapper);

      CM.getWrapperElement().onmouseleave = function(e) {
        CM.clearGutter("help-gutter");
      }

      // NOTE(joe): This seems to be the best way to get a hover on a mark: https://github.com/codemirror/CodeMirror/issues/3529
      CM.getWrapperElement().onmousemove = function(e) {
        var lineCh = CM.coordsChar({ left: e.clientX, top: e.clientY });
        var markers = CM.findMarksAt(lineCh);
        if (markers.length === 0) {
          CM.clearGutter("help-gutter");
        }
        if (lineCh.line === 0 && markers[0] === namespacemark) {
          CM.setGutterMarker(0, "help-gutter", gutterQuestionWrapper);
        }
        else {
          CM.clearGutter("help-gutter");
        }
      }
      CM.on("change", function(change) {
        function doesNotChangeFirstLine(c) { return c.from.line !== 0; }
        if(change.curOp.changeObjs && change.curOp.changeObjs.every(doesNotChangeFirstLine)) { return; }
        var hasNamespace = firstLineIsNamespace();
        if(hasNamespace) {
          if(namespacemark) { namespacemark.clear(); }
          namespacemark = CM.markText({line: 0, ch: 0}, {line: 1, ch: 0}, { attributes: { useline: true }, className: "useline", atomic: true, inclusiveLeft: true, inclusiveRight: false });
        }
      });
    }
    if (useLineNumbers) {
      CM.display.wrapper.appendChild(mkWarningUpper()[0]);
      CM.display.wrapper.appendChild(mkWarningLower()[0]);
    }

    getTopTierMenuitems();

    return {
      cm: CM,
      setContextLine: setContextLine,
      refresh: function() { CM.refresh(); },
      run: function() {
        runFun(CM.getValue());
      },
      focus: function() { CM.focus(); },
      focusCarousel: null //initFocusCarousel
    };
  };
  CPO.RUN_CODE = function() {
    console.log("Running before ready", arguments);
  };

  function setUsername(target) {
    var token = window.gapi.auth.getToken().access_token;
    return fetch('https://openidconnect.googleapis.com/v1/userinfo', {
      headers: { Authorization: 'Bearer ' + token }
    }).then(function(resp) {
      return resp.json();
    }).then(function(info) {
      target.text(info.email);
    });
  }

  storageAPI.then(function(api) {
    api.collection.then(function() {
      $(".loginOnly").show();
      $(".logoutOnly").hide();
      setUsername($("#username"));
    });
    api.collection.fail(function() {
      $(".loginOnly").hide();
      $(".logoutOnly").show();
    });
  });

  storageAPI = storageAPI.then(function(api) { return api.api; });
  $("#connectButton").click(function() {
    $("#connectButton").text("Connecting...");
    $("#connectButton").attr("disabled", "disabled");
    $('#connectButtonli').attr('disabled', 'disabled');
    $("#connectButton").attr("tabIndex", "-1");
    //$("#topTierUl").attr("tabIndex", "0");
    getTopTierMenuitems();
    storageAPI = createProgramCollectionAPI(process.env.APP_NAME, false);
    storageAPI.then(function(api) {
      api.collection.then(function() {
        $(".loginOnly").show();
        $(".logoutOnly").hide();
        document.activeElement.blur();
        $("#bonniemenubutton").focus();
        setUsername($("#username"));
        if(params["get"] && params["get"]["program"]) {
          var toLoad = api.api.getFileById(params["get"]["program"]);
          console.log("Logged in and has program to load: ", toLoad);
          loadProgram(toLoad);
          programToSave = toLoad;
        } else {
          programToSave = Q.fcall(function() { return null; });
        }
      });
      api.collection.fail(function() {
        $("#connectButton").text("Connect to Google Drive");
        $("#connectButton").attr("disabled", false);
        $('#connectButtonli').attr('disabled', false);
        //$("#connectButton").attr("tabIndex", "0");
        document.activeElement.blur();
        $("#connectButton").focus();
        //$("#topTierUl").attr("tabIndex", "-1");
      });
    });
    storageAPI = storageAPI.then(function(api) { return api.api; });
  });

  /*
    initialProgram holds a promise for a Drive File object or null

    It's null if the page doesn't have a #share or #program url

    If the url does have a #program or #share, the promise is for the
    corresponding object.
  */
  let initialProgram;
  if(params["get"] && params["get"]["shareurl"]) {
    initialProgram = makeUrlFile(params["get"]["shareurl"]);
  }
  else {
    initialProgram = storageAPI.then(function(api) {
      var programLoad = null;
      if(params["get"] && params["get"]["program"]) {
        enableFileOptions();
        programLoad = api.getFileById(params["get"]["program"]);
        programLoad.then(function(p) { showShareContainer(p); });
      }
      else if(params["get"] && params["get"]["share"]) {
        logger.log('shared-program-load',
          {
            id: params["get"]["share"]
          });
        programLoad = api.getSharedFileById(params["get"]["share"]);
        programLoad.then(function(file) {
          // NOTE(joe): If the current user doesn't own or have access to this file
          // (or isn't logged in) this will simply fail with a 401, so we don't do
          // any further permission checking before showing the link.
          file.getOriginal().then(function(response) {
            console.log("Response for original: ", response);
            var original = $("#open-original").show().off("click");
            var id = response.result.value;
            original.removeClass("hidden");
            original.click(function() {
              window.open(window.APP_BASE_URL + "/editor#program=" + id, "_blank");
            });
          });
        });
      }
      else {
        programLoad = null;
      }
      if(programLoad) {
        programLoad.fail(function(err) {
          console.error(err);
          window.stickError("The program failed to load.");
        });
        return programLoad;
      } else {
        return null;
      }
    }).catch(e => {
      console.error("storageAPI failed to load, proceeding without saving programs: ", e);
      return null;
    });
  }

  function setTitle(progName) {
    document.title = progName + " - " + process.env.APP_NAME;
    $("#showFilename").text("File: " + progName);
  }
  CPO.setTitle = setTitle;

  var filename = false;

  $("#download a").click(function() {
    var downloadElt = $("#download a");
    var contents = CPO.editor.cm.getValue();
    var downloadBlob = window.URL.createObjectURL(new Blob([contents], {type: 'text/plain'}));
    if(!filename) { filename = 'untitled_program.arr'; }
    if(filename.indexOf(".arr") !== (filename.length - 4)) {
      filename += ".arr";
    }
    downloadElt.attr({
      download: filename,
      href: downloadBlob
    });
    $("#download").append(downloadElt);
  });

  function showModal(currentContext) {
    function drawElement(input) {
      const element = $("<div>");
      const greeting = $("<p>");
      const shared = $("<tt>shared-gdrive(...)</tt>");
      const currentContextElt = $("<tt>" + currentContext + "</tt>");
      greeting.append("Enter the context to use for the program, or choose “Cancel” to keep the current context of ", currentContextElt, ".");
      const essentials = $("<tt>starter2024</tt>");
      const list = $("<ul>")
        .append($("<li>").append("The default is ", essentials, "."))
        .append($("<li>").append("You might use something like ", shared, " if one was provided as part of a course."));
      element.append(greeting);
      element.append($("<p>").append(list));
      const useContext = $("<tt>use context</tt>").css({ 'flex-grow': '0', 'padding-right': '1em' });
      const inputWrapper = $("<div>").append(input).css({ 'flex-grow': '1' });
      const entry = $("<div>").css({
        display: 'flex',
        'flex-direction': 'row',
        'justify-content': 'flex-start',
        'align-items': 'baseline'
      });
      entry.append(useContext).append(inputWrapper);
      element.append(entry);
      return element;
    }
    const namespaceResult = new modalPrompt({
        title: "Choose a Context",
        style: "text",
        options: [
          {
            drawElement: drawElement,
            submitText: "Change Namespace",
            defaultValue: currentContext
          }
        ]
      });
    namespaceResult.show((result) => {
      if(!result) { return; }
      CPO.editor.setContextLine("use context " + result.trim() + "\n");
    });
  }
  $("#choose-context").on("click", function() {
    const firstLine = CPO.editor.cm.getLine(0);
    const contextLen = firstLine.match(CONTEXT_PREFIX);
    showModal(contextLen === null ? "" : firstLine.slice(contextLen[0].length));
  });

  var TRUNCATE_LENGTH = 20;

  function truncateName(name) {
    if(name.length <= TRUNCATE_LENGTH + 1) { return name; }
    return name.slice(0, TRUNCATE_LENGTH / 2) + "…" + name.slice(name.length - TRUNCATE_LENGTH / 2, name.length);
  }

  function updateName(p) {
    filename = p.getName();
    $("#filename").text(" (" + truncateName(filename) + ")");
    $("#filename").attr('title', filename);
    setTitle(filename);
    showShareContainer(p);
  }

  function loadProgram(p) {
    programToSave = p;
    return p.then(function(prog) {
      if(prog !== null) {
        updateName(prog);
        if(prog.shared) {
          window.stickMessage("You are viewing a shared program. Any changes you make will not be saved. You can use File -> Save a copy to save your own version with any edits you make.");
        }
        return prog.getContents();
      }
      else {
        if(params["get"]["editorContents"] && !(params["get"]["program"] || params["get"]["share"])) {
          return params["get"]["editorContents"];
        }
        else {
          return CONTEXT_FOR_NEW_FILES;
        }
      }
    });
  }

  function say(msg, forget) {
    if (msg === "") return;
    var announcements = document.getElementById("announcementlist");
    var li = document.createElement("LI");
    li.appendChild(document.createTextNode(msg));
    announcements.insertBefore(li, announcements.firstChild);
    if (forget) {
      setTimeout(function() {
        announcements.removeChild(li);
      }, 1000);
    }
  }

  function sayAndForget(msg) {
    console.log('doing sayAndForget', msg);
    say(msg, true);
  }

  function cycleAdvance(currIndex, maxIndex, reverseP) {
    var nextIndex = currIndex + (reverseP? -1 : +1);
    nextIndex = ((nextIndex % maxIndex) + maxIndex) % maxIndex;
    return nextIndex;
  }

  function populateFocusCarousel(editor) {
    if (!editor.focusCarousel) {
      editor.focusCarousel = [];
    }
    var fc = editor.focusCarousel;
    var docmain = document.getElementById("main");
    if (!fc[0]) {
      var toolbar = document.getElementById('Toolbar');
      fc[0] = toolbar;
      //fc[0] = document.getElementById("headeronelegend");
      //getTopTierMenuitems();
      //fc[0] = document.getElementById('bonniemenubutton');
    }
    if (!fc[1]) {
      var docreplMain = docmain.getElementsByClassName("replMain");
      var docreplMain0;
      if (docreplMain.length === 0) {
        docreplMain0 = undefined;
      } else if (docreplMain.length === 1) {
        docreplMain0 = docreplMain[0];
      } else {
        for (var i = 0; i < docreplMain.length; i++) {
          if (docreplMain[i].innerText !== "") {
            docreplMain0 = docreplMain[i];
          }
        }
      }
      fc[1] = docreplMain0;
    }
    if (!fc[2]) {
      var docrepl = docmain.getElementsByClassName("repl");
      var docreplcode = docrepl[0].getElementsByClassName("prompt-container")[0].
        getElementsByClassName("CodeMirror")[0];
      fc[2] = docreplcode;
    }
    if (!fc[3]) {
      fc[3] = document.getElementById("announcements");
    }
  }

  function cycleFocus(reverseP) {
    //console.log('doing cycleFocus', reverseP);
    var editor = this.editor;
    populateFocusCarousel(editor);
    var fCarousel = editor.focusCarousel;
    var maxIndex = fCarousel.length;
    var currentFocusedElt = fCarousel.find(function(node) {
      if (!node) {
        return false;
      } else {
        return node.contains(document.activeElement);
      }
    });
    var currentFocusIndex = fCarousel.indexOf(currentFocusedElt);
    var nextFocusIndex = currentFocusIndex;
    var focusElt;
    do {
      nextFocusIndex = cycleAdvance(nextFocusIndex, maxIndex, reverseP);
      focusElt = fCarousel[nextFocusIndex];
      //console.log('trying focusElt', focusElt);
    } while (!focusElt);

    var focusElt0;
    if (focusElt.classList.contains('toolbarregion')) {
      //console.log('settling on toolbar region')
      getTopTierMenuitems();
      focusElt0 = document.getElementById('bonniemenubutton');
    } else if (focusElt.classList.contains("replMain") ||
      focusElt.classList.contains("CodeMirror")) {
      //console.log('settling on defn window')
      var textareas = focusElt.getElementsByTagName("textarea");
      //console.log('txtareas=', textareas)
      //console.log('txtarea len=', textareas.length)
      if (textareas.length === 0) {
        //console.log('I')
        focusElt0 = focusElt;
      } else if (textareas.length === 1) {
        //console.log('settling on inter window')
        focusElt0 = textareas[0];
      } else {
        //console.log('settling on defn window')
        /*
        for (var i = 0; i < textareas.length; i++) {
          if (textareas[i].getAttribute('tabIndex')) {
            focusElt0 = textareas[i];
          }
        }
        */
        focusElt0 = textareas[textareas.length-1];
        focusElt0.removeAttribute('tabIndex');
      }
    } else {
      //console.log('settling on announcement region', focusElt)
      focusElt0 = focusElt;
    }

    document.activeElement.blur();
    focusElt0.click();
    focusElt0.focus();
    //console.log('(cf)docactelt=', document.activeElement);
  }

  var programLoaded = loadProgram(initialProgram);

  var programToSave = initialProgram;

  function showShareContainer(p) {
    //console.log('called showShareContainer');
    if(!p.shared) {
      $("#shareContainer").empty();
      $('#publishli').show();
      $("#shareContainer").append(shareAPI.makeShareLink(p));
      getTopTierMenuitems();
    }
  }

  function nameOrUntitled() {
    return filename || "Untitled";
  }
  function autoSave() {
    programToSave.then(function(p) {
      if(p !== null && !p.shared) { save(); }
    });
  }

  function enableFileOptions() {
    $("#filemenuContents *").removeClass("disabled");
  }

  function menuItemDisabled(id) {
    return $("#" + id).hasClass("disabled");
  }

  function newEvent(e) {
    window.open(window.APP_BASE_URL + "/editor");
  }

  function saveEvent(e) {
    if(menuItemDisabled("save")) { return; }
    return save();
  }

  /*
    save : string (optional) -> undef

    If a string argument is provided, create a new file with that name and save
    the editor contents in that file.

    If no filename is provided, save the existing file referenced by the editor
    with the current editor contents.  If no filename has been set yet, just
    set the name to "Untitled".

  */
  function save(newFilename) {
    var useName, create;
    if(newFilename !== undefined) {
      useName = newFilename;
      create = true;
    }
    else if(filename === false) {
      filename = "Untitled";
      create = true;
    }
    else {
      useName = filename; // A closed-over variable
      create = false;
    }
    window.stickMessage("Saving...");
    var savedProgram = programToSave.then(function(p) {
      if(p !== null && p.shared && !create) {
        return p; // Don't try to save shared files
      }
      if(create) {
        programToSave = storageAPI
          .then(function(api) { return api.createFile(useName); })
          .then(function(p) {
            // showShareContainer(p); TODO(joe): figure out where to put this
            history.pushState(null, null, "#program=" + p.getUniqueId());
            updateName(p); // sets filename
            enableFileOptions();
            return p;
          });
        return programToSave.then(function(p) {
          return save();
        });
      }
      else {
        return programToSave.then(function(p) {
          if(p === null) {
            return null;
          }
          else {
            return p.save(CPO.editor.cm.getValue(), false);
          }
        }).then(function(p) {
          if(p !== null) {
            window.flashMessage("Program saved as " + p.getName());
          }
          return p;
        });
      }
    });
    savedProgram.fail(function(err) {
      window.stickError("Unable to save", "Your internet connection may be down, or something else might be wrong with this site or saving to Google.  You should back up any changes to this program somewhere else.  You can try saving again to see if the problem was temporary, as well.");
      console.error(err);
    });
    return savedProgram;
  }

  function saveAs() {
    if(menuItemDisabled("saveas")) { return; }
    programToSave.then(function(p) {
      var name = p === null ? "Untitled" : p.getName();
      var saveAsPrompt = new modalPrompt({
        title: "Save a copy",
        style: "text",
        submitText: "Save",
        narrow: true,
        options: [
          {
            message: "The name for the copy:",
            defaultValue: name
          }
        ]
      });
      return saveAsPrompt.show().then(function(newName) {
        if(newName === null) { return null; }
        window.stickMessage("Saving...");
        return save(newName);
      }).
      fail(function(err) {
        console.error("Failed to rename: ", err);
        window.flashError("Failed to rename file");
      });
    });
  }

  function rename() {
    programToSave.then(function(p) {
      var renamePrompt = new modalPrompt({
        title: "Rename this file",
        style: "text",
        narrow: true,
        submitText: "Rename",
        options: [
          {
            message: "The new name for the file:",
            defaultValue: p.getName()
          }
        ]
      });
      // null return values are for the "cancel" path
      return renamePrompt.show().then(function(newName) {
        if(newName === null) {
          return null;
        }
        window.stickMessage("Renaming...");
        programToSave = p.rename(newName);
        return programToSave;
      })
      .then(function(p) {
        if(p === null) {
          return null;
        }
        updateName(p);
        window.flashMessage("Program saved as " + p.getName());
      })
      .fail(function(err) {
        console.error("Failed to rename: ", err);
        window.flashError("Failed to rename file");
      });
    })
    .fail(function(err) {
      console.error("Unable to rename: ", err);
    });
  }

  $("#runButton").click(function() {
    CPO.autoSave();
  });

  $("#new").click(newEvent);
  $("#save").click(saveEvent);
  $("#rename").click(rename);
  $("#saveas").click(saveAs);

  var focusableElts = $(document).find('#header .focusable');
  //console.log('focusableElts=', focusableElts)
  var theToolbar = $(document).find('#Toolbar');

  function getTopTierMenuitems() {
    //console.log('doing getTopTierMenuitems')
    var topTierMenuitems = $(document).find('#header ul li.topTier').toArray();
    topTierMenuitems = topTierMenuitems.
                        filter(elt => !(elt.style.display === 'none' ||
                                        elt.getAttribute('disabled') === 'disabled'));
    var numTopTierMenuitems = topTierMenuitems.length;
    for (var i = 0; i < numTopTierMenuitems; i++) {
      var ithTopTierMenuitem = topTierMenuitems[i];
      var iChild = $(ithTopTierMenuitem).children().first();
      //console.log('iChild=', iChild);
      iChild.find('.focusable').
        attr('aria-setsize', numTopTierMenuitems.toString()).
        attr('aria-posinset', (i+1).toString());
    }
    return topTierMenuitems;
  }

  function updateEditorHeight() {
    var toolbarHeight = document.getElementById('topTierUl').offsetHeight;
    // gets bumped to 67 on initial resize perturbation, but actual value is indeed 40
    if (toolbarHeight < 80) toolbarHeight = 40;
    toolbarHeight += 'px';
    document.getElementById('REPL').style.paddingTop = toolbarHeight;
    var docMain = document.getElementById('main');
    var docReplMain = docMain.getElementsByClassName('replMain');
    if (docReplMain.length !== 0) {
      docReplMain[0].style.paddingTop = toolbarHeight;
    }
  }

  $(window).on('resize', updateEditorHeight);

  function insertAriaPos(submenu) {
    //console.log('doing insertAriaPos', submenu)
    var arr = submenu.toArray();
    //console.log('arr=', arr);
    var len = arr.length;
    for (var i = 0; i < len; i++) {
      var elt = arr[i];
      //console.log('elt', i, '=', elt);
      elt.setAttribute('aria-setsize', len.toString());
      elt.setAttribute('aria-posinset', (i+1).toString());
    }
  }


  document.addEventListener('click', function () {
    hideAllTopMenuitems();
  });

  theToolbar.click(function (e) {
    e.stopPropagation();
  });

  theToolbar.keydown(function (e) {
    //console.log('toolbar keydown', e);
    //most any key at all
    var kc = e.keyCode;
    if (kc === 27) {
      // escape
      hideAllTopMenuitems();
      //console.log('calling cycleFocus from toolbar')
      CPO.cycleFocus();
      e.stopPropagation();
    } else if (kc === 9 || kc === 37 || kc === 38 || kc === 39 || kc === 40) {
      // an arrow
      var target = $(this).find('[tabIndex=-1]');
      getTopTierMenuitems();
      document.activeElement.blur(); //needed?
      target.first().focus(); //needed?
      //console.log('docactelt=', document.activeElement);
      e.stopPropagation();
    } else {
      hideAllTopMenuitems();
    }
  });

  function clickTopMenuitem(e) {
    hideAllTopMenuitems();
    var thisElt = $(this);
    //console.log('doing clickTopMenuitem on', thisElt);
    var topTierUl = thisElt.closest('ul[id=topTierUl]');
    if (thisElt[0].hasAttribute('aria-hidden')) {
      return;
    }
    if (thisElt[0].getAttribute('disabled') === 'disabled') {
      return;
    }
    //var hiddenP = (thisElt[0].getAttribute('aria-expanded') === 'false');
    //hiddenP always false?
    var thisTopMenuitem = thisElt.closest('li.topTier');
    //console.log('thisTopMenuitem=', thisTopMenuitem);
    var t1 = thisTopMenuitem[0];
    var submenuOpen = (thisElt[0].getAttribute('aria-expanded') === 'true');
    if (!submenuOpen) {
      //console.log('hiddenp true branch');
      hideAllTopMenuitems();
      thisTopMenuitem.children('ul.submenu').attr('aria-hidden', 'false').show();
      thisTopMenuitem.children().first().find('[aria-expanded]').attr('aria-expanded', 'true');
    } else {
      //console.log('hiddenp false branch');
      thisTopMenuitem.children('ul.submenu').attr('aria-hidden', 'true').hide();
      thisTopMenuitem.children().first().find('[aria-expanded]').attr('aria-expanded', 'false');
    }
    e.stopPropagation();
  }

  var expandableElts = $(document).find('#header [aria-expanded]');
  expandableElts.click(clickTopMenuitem);

  function hideAllTopMenuitems() {
    //console.log('doing hideAllTopMenuitems');
    var topTierUl = $(document).find('#header ul[id=topTierUl]');
    topTierUl.find('[aria-expanded]').attr('aria-expanded', 'false');
    topTierUl.find('ul.submenu').attr('aria-hidden', 'true').hide();
  }

  var nonexpandableElts = $(document).find('#header .topTier > div > button:not([aria-expanded])');
  nonexpandableElts.click(hideAllTopMenuitems);

  function switchTopMenuitem(destTopMenuitem, destElt) {
    //console.log('doing switchTopMenuitem', destTopMenuitem, destElt);
    //console.log('dtmil=', destTopMenuitem.length);
    hideAllTopMenuitems();
    if (destTopMenuitem && destTopMenuitem.length !== 0) {
      var elt = destTopMenuitem[0];
      var eltId = elt.getAttribute('id');
      destTopMenuitem.children('ul.submenu').attr('aria-hidden', 'false').show();
      destTopMenuitem.children().first().find('[aria-expanded]').attr('aria-expanded', 'true');
    }
    if (destElt) {
      //destElt.attr('tabIndex', '0').focus();
      destElt.focus();
    }
  }

  var showingHelpKeys = false;

  function showHelpKeys() {
    showingHelpKeys = true;
    $('#help-keys').fadeIn(100);
    reciteHelp();
  }

  focusableElts.keydown(function (e) {
    //console.log('focusable elt keydown', e);
    var kc = e.keyCode;
    //$(this).blur(); // Delete?
    var withinSecondTierUl = true;
    var topTierUl = $(this).closest('ul[id=topTierUl]');
    var secondTierUl = $(this).closest('ul.submenu');
    if (secondTierUl.length === 0) {
      withinSecondTierUl = false;
    }
    if (kc === 27) {
      //console.log('escape pressed i')
      $('#help-keys').fadeOut(500);
    }
    if (kc === 27 && withinSecondTierUl) { // escape
      var destTopMenuitem = $(this).closest('li.topTier');
      var possElts = destTopMenuitem.find('.focusable:not([disabled])').filter(':visible');
      switchTopMenuitem(destTopMenuitem, possElts.first());
      e.stopPropagation();
    } else if (kc === 39) { // rightarrow
      //console.log('rightarrow pressed');
      var srcTopMenuitem = $(this).closest('li.topTier');
      //console.log('srcTopMenuitem=', srcTopMenuitem);
      srcTopMenuitem.children().first().find('.focusable').attr('tabIndex', '-1');
      var topTierMenuitems = getTopTierMenuitems();
      //console.log('ttmi* =', topTierMenuitems);
      var ttmiN = topTierMenuitems.length;
      var j = topTierMenuitems.indexOf(srcTopMenuitem[0]);
      //console.log('j initial=', j);
      for (var i = (j + 1) % ttmiN; i !== j; i = (i + 1) % ttmiN) {
        var destTopMenuitem = $(topTierMenuitems[i]);
        //console.log('destTopMenuitem(a)=', destTopMenuitem);
        var possElts = destTopMenuitem.find('.focusable:not([disabled])').filter(':visible');
        //console.log('possElts=', possElts)
        if (possElts.length > 0) {
          //console.log('final i=', i);
          //console.log('landing on', possElts.first());
          switchTopMenuitem(destTopMenuitem, possElts.first());
          e.stopPropagation();
          break;
        }
      }
    } else if (kc === 37) { // leftarrow
      //console.log('leftarrow pressed');
      var srcTopMenuitem = $(this).closest('li.topTier');
      //console.log('srcTopMenuitem=', srcTopMenuitem);
      srcTopMenuitem.children().first().find('.focusable').attr('tabIndex', '-1');
      var topTierMenuitems = getTopTierMenuitems();
      //console.log('ttmi* =', topTierMenuitems);
      var ttmiN = topTierMenuitems.length;
      var j = topTierMenuitems.indexOf(srcTopMenuitem[0]);
      //console.log('j initial=', j);
      for (var i = (j + ttmiN - 1) % ttmiN; i !== j; i = (i + ttmiN - 1) % ttmiN) {
        var destTopMenuitem = $(topTierMenuitems[i]);
        //console.log('destTopMenuitem(b)=', destTopMenuitem);
        //console.log('i=', i)
        var possElts = destTopMenuitem.find('.focusable:not([disabled])').filter(':visible');
        //console.log('possElts=', possElts)
        if (possElts.length > 0) {
          //console.log('final i=', i);
          //console.log('landing on', possElts.first());
          switchTopMenuitem(destTopMenuitem, possElts.first());
          e.stopPropagation();
          break;
        }
      }
    } else if (kc === 38) { // uparrow
      //console.log('uparrow pressed');
      var submenu;
      if (withinSecondTierUl) {
        var nearSibs = $(this).closest('div').find('.focusable').filter(':visible');
        //console.log('nearSibs=', nearSibs);
        var myId = $(this)[0].getAttribute('id');
        //console.log('myId=', myId);
        submenu = $([]);
        var thisEncountered = false;
        for (var i = nearSibs.length - 1; i >= 0; i--) {
          if (thisEncountered) {
            //console.log('adding', nearSibs[i]);
            submenu = submenu.add($(nearSibs[i]));
          } else if (nearSibs[i].getAttribute('id') === myId) {
            thisEncountered = true;
          }
        }
        //console.log('submenu so far=', submenu);
        var farSibs = $(this).closest('li').prevAll().find('div:not(.disabled)')
          .find('.focusable').filter(':visible');
        submenu = submenu.add(farSibs);
        if (submenu.length === 0) {
          submenu = $(this).closest('li').closest('ul').find('div:not(.disabled)')
          .find('.focusable').filter(':visible').last();
        }
        if (submenu.length > 0) {
          submenu.last().focus();
        } else {
          /*
          //console.log('no actionable submenu found')
          var topmenuItem = $(this).closest('ul.submenu').closest('li')
          .children().first().find('.focusable:not([disabled])').filter(':visible');
          if (topmenuItem.length > 0) {
            topmenuItem.first().focus();
          } else {
            //console.log('no actionable topmenuitem found either')
          }
          */
        }
      }
      e.stopPropagation();
    } else if (kc === 40) { // downarrow
      //console.log('downarrow pressed');
      var submenuDivs;
      var submenu;
      if (!withinSecondTierUl) {
        //console.log('1st tier')
        submenuDivs = $(this).closest('li').children('ul').find('div:not(.disabled)');
        submenu = submenuDivs.find('.focusable').filter(':visible');
        insertAriaPos(submenu);
      } else {
        //console.log('2nd tier')
        var nearSibs = $(this).closest('div').find('.focusable').filter(':visible');
        //console.log('nearSibs=', nearSibs);
        var myId = $(this)[0].getAttribute('id');
        //console.log('myId=', myId);
        submenu = $([]);
        var thisEncountered = false;
        for (var i = 0; i < nearSibs.length; i++) {
          if (thisEncountered) {
            //console.log('adding', nearSibs[i]);
            submenu = submenu.add($(nearSibs[i]));
          } else if (nearSibs[i].getAttribute('id') === myId) {
            thisEncountered = true;
          }
        }
        //console.log('submenu so far=', submenu);
        var farSibs = $(this).closest('li').nextAll().find('div:not(.disabled)')
          .find('.focusable').filter(':visible');
        submenu = submenu.add(farSibs);
        if (submenu.length === 0) {
          submenu = $(this).closest('li').closest('ul').find('div:not(.disabled)')
            .find('.focusable').filter(':visible');
        }
      }
      //console.log('submenu=', submenu)
      if (submenu.length > 0) {
        submenu.first().focus();
      } else {
        //console.log('no actionable submenu found')
      }
      e.stopPropagation();
    } else if (kc === 27) {
      //console.log('esc pressed');
      hideAllTopMenuitems();
      if (showingHelpKeys) {
        showingHelpKeys = false;
      } else {
        //console.log('calling cycleFocus ii')
        CPO.cycleFocus();
      }
      e.stopPropagation();
      e.preventDefault();
      //$(this).closest('nav').closest('main').focus();
    } else if (kc === 9 ) {
      if (e.shiftKey) {
        hideAllTopMenuitems();
        CPO.cycleFocus(true);
      }
      e.stopPropagation();
      e.preventDefault();
    } else if (kc === 13 || kc === 17 || kc === 20 || kc === 32) {
      // 13=enter 17=ctrl 20=capslock 32=space
      //console.log('stopprop 1')
      e.stopPropagation();
    } else if (kc >= 112 && kc <= 123) {
      //console.log('doprop 1')
      // fn keys
      // go ahead, propagate
    } else if (e.ctrlKey && kc === 191) {
      //console.log('C-? pressed')
      showHelpKeys();
      e.stopPropagation();
    } else {
      //console.log('stopprop 2')
      e.stopPropagation();
    }
    //e.stopPropagation();
  });

  // shareAPI.makeHoverMenu($("#filemenu"), $("#filemenuContents"), false, function(){});
  // shareAPI.makeHoverMenu($("#bonniemenu"), $("#bonniemenuContents"), false, function(){});


  var codeContainer = $("<div>").addClass("replMain");
  codeContainer.attr("role", "region").
    attr("aria-label", "Definitions");
    //attr("tabIndex", "-1");
  $("#main").prepend(codeContainer);


  if(params["get"]["hideDefinitions"]) {
    $(".replMain").attr("aria-hidden", true).attr("tabindex", '-1');
  }
  
  const isControlled = params["get"]["controlled"];
  const hasWarnOnExit = ("warnOnExit" in params["get"]);
  const skipWarning = hasWarnOnExit && (params["get"]["warnOnExit"] === "false");

  if(!isControlled && !skipWarning) {
    $(window).bind("beforeunload", function() {
      return "Because this page can load slowly, and you may have outstanding changes, we ask that you confirm before leaving the editor in case closing was an accident.";
    });
  }

  CPO.editor = CPO.makeEditor(codeContainer, {
    runButton: $("#runButton"),
    simpleEditor: false,
    run: CPO.RUN_CODE,
    initialGas: 100,
    scrollPastEnd: true,
  });
  CPO.editor.cm.setOption("readOnly", "nocursor");
  CPO.editor.cm.setOption("longLines", new Map());
  function removeShortenedLine(lineHandle) {
    var rulers = CPO.editor.cm.getOption("rulers");
    var rulersMinCol = CPO.editor.cm.getOption("rulersMinCol");
    var longLines = CPO.editor.cm.getOption("longLines");
    if (lineHandle.text.length <= rulersMinCol) {
      lineHandle.rulerListeners.forEach((f, evt) => lineHandle.off(evt, f));
      longLines.delete(lineHandle);
      // console.log("Removed ", lineHandle);
      refreshRulers();
    }
  }
  function deleteLine(lineHandle) {
    var longLines = CPO.editor.cm.getOption("longLines");
    lineHandle.rulerListeners.forEach((f, evt) => lineHandle.off(evt, f));
    longLines.delete(lineHandle);
    // console.log("Removed ", lineHandle);
    refreshRulers();
  }
  function refreshRulers() {
    var rulers = CPO.editor.cm.getOption("rulers");
    var longLines = CPO.editor.cm.getOption("longLines");
    var minLength;
    if (longLines.size === 0) {
      minLength = 0; // if there are no long lines, then we don't care about showing any rulers
    } else {
      minLength = Number.MAX_VALUE;
      longLines.forEach(function(lineNo, lineHandle) {
        if (lineHandle.text.length < minLength) { minLength = lineHandle.text.length; }
      });
    }
    for (var i = 0; i < rulers.length; i++) {
      if (rulers[i].column >= minLength) {
        rulers[i].className = "hidden";
      } else {
        rulers[i].className = undefined;
      }
    }
    // gotta set the option twice, or else CM short-circuits and ignores it
    CPO.editor.cm.setOption("rulers", undefined);
    CPO.editor.cm.setOption("rulers", rulers);
  }
  CPO.editor.cm.on('changes', function(instance, changeObjs) {
    var minLine = instance.lastLine(), maxLine = 0;
    var rulersMinCol = instance.getOption("rulersMinCol");
    var longLines = instance.getOption("longLines");
    changeObjs.forEach(function(change) {
      if (minLine > change.from.line) { minLine = change.from.line; }
      if (maxLine < change.from.line + change.text.length) { maxLine = change.from.line + change.text.length; }
    });
    var changed = false;
    instance.eachLine(minLine, maxLine, function(lineHandle) {
      if (lineHandle.text.length > rulersMinCol) {
        if (!longLines.has(lineHandle)) {
          changed = true;
          longLines.set(lineHandle, lineHandle.lineNo());
          lineHandle.rulerListeners = new Map([
            ["change", removeShortenedLine],
            ["delete", function() { // needed because the delete handler gets no arguments at all
              deleteLine(lineHandle);
            }]
          ]);
          lineHandle.rulerListeners.forEach((f, evt) => lineHandle.on(evt, f));
          // console.log("Added ", lineHandle);
        }
      } else {
        if (longLines.has(lineHandle)) {
          changed = true;
          longLines.delete(lineHandle);
          // console.log("Removed ", lineHandle);
        }
      }
    });
    if (changed) {
      refreshRulers();
    }
  });

  programLoaded.then(function(c) {
    CPO.documents.set("definitions://", CPO.editor.cm.getDoc());
    if(c === "") {
      c = CONTEXT_FOR_NEW_FILES;
    }

    if (c.startsWith("<scriptsonly")) {
      // this is blocks file. Open it with /blocks
      window.location.href = window.location.href.replace('editor', 'blocks');
    }

    if(!params["get"]["controlled"]) {
      // NOTE(joe): Clearing history to address https://github.com/brownplt/pyret-lang/issues/386,
      // in which undo can revert the program back to empty
      CPO.editor.cm.setValue(c);
      CPO.editor.cm.clearHistory();
    }
    else {
      const hideWhenControlled = [
        "#logging",
        "#logout"
      ];
      const removeWhenControlled = [
        "#connectButtonli",
      ];
      hideWhenControlled.forEach(s => $(s).hide());
      removeWhenControlled.forEach(s => $(s).remove());
    }

    // Standalone boot settles here; a host-fed editor settles at the end of
    // events.js reset() instead (see EXPECTS_HOST_RESET above).
    if(!window.EXPECTS_HOST_RESET) {
      window.EDITOR_CONTENTS_SETTLED = true;
    }

  });

  programLoaded.fail(function(error) {
    console.error("Program contents did not load: ", error);
    CPO.documents.set("definitions://", CPO.editor.cm.getDoc());
  });

  console.log("About to load Pyret: ", originalPageLoad, Date.now());

  // If the primary Pyret URL (usually a CDN) fails, we fall back to the
  // same-origin copy at PYRET_BACKUP (always the plain compiler). The
  // "error" event alone isn't enough to trigger that: school content
  // filters have been seen both silently stalling the request (neither
  // "load" nor "error" ever fires) and answering it with an empty 200
  // (which fires "load"!). So two extra signals count as failure: going
  // PYRET_LOAD_TIMEOUT_MS with no event, and a "load" after which the
  // bundle's globals aren't actually defined. When the primary fails
  // either way, we record it in localSettings; until that record expires,
  // page loads swap the two URLs and go straight to the backup rather
  // than failing over from scratch again. A successful load from the
  // primary clears the record.
  var PYRET_LOAD_TIMEOUT_MS = 20000;
  var PYRET_FAILED_KEY = "pyret-primary-failed-at";
  var PYRET_FAILED_TTL_MS = 24 * 60 * 60 * 1000;

  function primaryFailedRecently() {
    var stamp = Number(localSettings.getItem(PYRET_FAILED_KEY));
    return stamp > 0 && (Date.now() - stamp) < PYRET_FAILED_TTL_MS;
  }

  function recordPrimaryFailure() {
    if (primaryPyret === window.PYRET && !window.PYRET_GZIPPED && window.CPO_COMPILER !== "ts") {
      localSettings.setItem(PYRET_FAILED_KEY, String(Date.now()));
    }
  }

  // The bundle synchronously installs its module loader as `define` and
  // `requirejs`; if neither exists after a script's "load" event, whatever
  // the network handed us was not Pyret (e.g. a filter's empty 200).
  function pyretActuallyLoaded() {
    return !(typeof window.define === "undefined" && typeof window.requirejs === "undefined");
  }

  var primaryPyret = window.PYRET;
  var backupPyret = process.env.PYRET_BACKUP;
  // No swapping in the gzipped (webview) configuration, where the two URLs
  // are fetched by different mechanisms, or in the ts flavor, where the
  // primary is the ts jarr but every fallback goes to the plain compiler.
  if (!window.PYRET_GZIPPED && window.CPO_COMPILER !== "ts" && backupPyret && primaryFailedRecently()) {
    console.log("Primary Pyret URL failed recently; loading from backup first");
    primaryPyret = process.env.PYRET_BACKUP;
    backupPyret = window.PYRET;
  }

  var pyretLoad = document.createElement('script');
  console.log(primaryPyret);
  pyretLoad.type = "text/javascript";
  pyretLoad.setAttribute("crossorigin", "anonymous");

  var pyretLoad2 = document.createElement('script');

  if (window.PYRET_GZIPPED) {
    // The runtime bundle is gzipped and this host serves it WITHOUT an
    // executable MIME type or Content-Encoding (e.g. a vscode webview whose
    // resources come from Open VSX / the GitLab Web IDE). fetch ignores script
    // MIME, so pull the .gz.js and inflate it in-page with the native
    // DecompressionStream, then run it from a Blob URL. The `error` handler
    // registered below (synchronously) fires before this async append resolves.
    //
    // In the ts flavor the compiler bundle has the same MIME problem (its
    // <script src> in editor.html is skipped under PYRET_GZIPPED) and, like
    // the jarr, is gzip bytes at rest (ts-compiler.gz.js) that this host
    // serves without Content-Encoding -- so fetch, inflate, and Blob-execute
    // it FIRST: the jarr expects window.PyretTSCompiler, matching the
    // synchronous script order of the un-gzipped page.
    var tsCompilerLoad = Promise.resolve();
    if (window.CPO_COMPILER === "ts" && window.PYRET_TS_COMPILER) {
      tsCompilerLoad = fetch(window.PYRET_TS_COMPILER)
        .then(function (resp) {
          if (!resp.ok) { throw new Error("status " + resp.status); }
          return new Response(resp.body.pipeThrough(new DecompressionStream("gzip"))).blob();
        })
        .then(function (blob) {
          return new Promise(function (resolve, reject) {
            var tsLoad = document.createElement('script');
            tsLoad.onload = resolve;
            tsLoad.onerror = function () { reject(new Error("executing ts-compiler bundle failed")); };
            tsLoad.src = URL.createObjectURL(new Blob([blob], { type: "application/javascript" }));
            document.body.appendChild(tsLoad);
          });
        });
    }
    tsCompilerLoad
      .then(function () {
        return fetch(window.PYRET);
      })
      .then(function (resp) {
        if (!resp.ok) { throw new Error("status " + resp.status); }
        return new Response(resp.body.pipeThrough(new DecompressionStream("gzip"))).blob();
      })
      .then(function (blob) {
        // If the fetch stalled long enough for the timeout to fire, the
        // backup owns the page now; don't run a second copy.
        if (backupStarted) { return; }
        pyretLoad.src = URL.createObjectURL(new Blob([blob], { type: "application/javascript" }));
        document.body.appendChild(pyretLoad);
      })
      .catch(function (e) {
        clearTimeout(primaryTimer);
        logFailureAndManualFetch(window.PYRET, e);
        loadBackupPyret("fetching/decompressing " + window.PYRET + " failed: " + e.message);
      });
  } else {
    pyretLoad.src = primaryPyret;
    document.body.appendChild(pyretLoad);
  }

  var primaryTimer = setTimeout(function() {
    logger.log('pyret-load-failure', {
      event : 'timeout',
      url : primaryPyret,
      timeoutMs : PYRET_LOAD_TIMEOUT_MS
    });
    recordPrimaryFailure();
    // Removing the element abandons the stalled request; a script element
    // that is disconnected before it executes won't run, so a late arrival
    // can't execute a second copy of Pyret alongside the backup.
    pyretLoad.remove();
    loadBackupPyret("the request for " + primaryPyret + " went " + PYRET_LOAD_TIMEOUT_MS + "ms with neither load nor error (stalled?)");
  }, PYRET_LOAD_TIMEOUT_MS);

  $(pyretLoad).on("load", function() {
    clearTimeout(primaryTimer);
    if (!pyretActuallyLoaded()) {
      logger.log('pyret-load-failure', {
        event : 'empty-load',
        url : primaryPyret
      });
      recordPrimaryFailure();
      loadBackupPyret("the response for " + primaryPyret + " loaded without defining Pyret's globals (empty or replaced body?)");
      return;
    }
    if (primaryPyret === window.PYRET && localSettings.getItem(PYRET_FAILED_KEY)) {
      localSettings.setItem(PYRET_FAILED_KEY, "");
    }
  });

  // The page's terminal state: neither the runtime bundle nor its backup is
  // coming. Alongside the user-facing banner, say WHY on the console -- in a
  // vscode webview there is no logging server behind logger.log, so the
  // console line is the only diagnostic that survives (and the browser-test
  // harness now records it).
  function terminalPyretLoadFailure(detail) {
    console.error("Pyret failed to load: " + detail);
    $("#loader").hide();
    $("#runPart").hide();
    $("#breakButton").hide();
    window.stickError("Pyret failed to load; check your connection or try refreshing the page.  If this happens repeatedly, please report it as a bug.  (" + detail + ")");
  }

  var backupStarted = false;
  function loadBackupPyret(primaryDetail) {
    console.error("Pyret runtime bundle failed to load: " + primaryDetail);
    if (backupStarted) { return; }
    backupStarted = true;
    // Builds without a configured PYRET_BACKUP (the vscode webview, anything
    // built without the env var) used to assign it anyway, so the browser
    // requested a literal "undefined" -- an instant 404 whose error event
    // replaced the primary failure's story. No backup: go straight to the
    // terminal state, carrying the reason the primary died.
    if (!backupPyret) {
      terminalPyretLoadFailure(primaryDetail);
      return;
    }
    var backupTimer = setTimeout(function() {
      // The backup request is left in flight, so if it does eventually
      // finish, the page still becomes usable under the banner.
      logger.log('pyret-load-failure', {
        event : 'timeout',
        url : backupPyret,
        timeoutMs : PYRET_LOAD_TIMEOUT_MS
      });
      terminalPyretLoadFailure("the backup bundle " + backupPyret + " also went " + PYRET_LOAD_TIMEOUT_MS + "ms with neither load nor error");
    }, PYRET_LOAD_TIMEOUT_MS);
    $(pyretLoad2).on("load", function() {
      clearTimeout(backupTimer);
      if (!pyretActuallyLoaded()) {
        logger.log('pyret-load-failure', {
          event : 'empty-load',
          url : backupPyret
        });
        terminalPyretLoadFailure("the backup bundle " + backupPyret + " loaded without defining Pyret's globals");
      }
    });
    pyretLoad2.src = backupPyret;
    pyretLoad2.type = "text/javascript";
    document.body.appendChild(pyretLoad2);
  }

  function logFailureAndManualFetch(url, e) {

    // NOTE(joe): The error reported by the "error" event has essentially no
    // information on it; it's just a notification that _something_ went wrong.
    // So, we log that something happened, then immediately do an AJAX request
    // call for the same URL, to see if we can get more information. This
    // doesn't perfectly tell us about the original failure, but it's
    // something.

    // In addition, if someone is seeing the Pyret failed to load error, but we
    // don't get these logging events, we have a strong hint that something is
    // up with their network.
    logger.log('pyret-load-failure',
      {
        event : 'initial-failure',
        url : url,

        // The timestamp appears to count from the beginning of page load,
        // which may approximate download time if, say, requests are timing out
        // or getting cut off.

        timeStamp : e.timeStamp
      });

    var manualFetch = $.ajax(url);
    manualFetch.then(function(res) {
      // Here, we log the first 100 characters of the response to make sure
      // they resemble the Pyret blob
      logger.log('pyret-load-failure', {
        event : 'success-with-ajax',
        contentsPrefix : res.slice(0, 100)
      });
    });
    manualFetch.fail(function(res) {
      logger.log('pyret-load-failure', {
        event : 'failure-with-ajax',
        status: res.status,
        statusText: res.statusText,
        // Since responseText could be a long error page, and we don't want to
        // log huge pages, we slice it to 100 characters, which is enough to
        // tell us what's going on (e.g. AWS failure, network outage).
        responseText: res.responseText.slice(0, 100)
      });
    });
  }

  $(pyretLoad).on("error", function(e) {
    clearTimeout(primaryTimer);
    logFailureAndManualFetch(primaryPyret, e);
    loadBackupPyret("the script tag for " + primaryPyret + " fired its error event");
  });

  $(pyretLoad2).on("error", function(e) {
    terminalPyretLoadFailure("the backup bundle " + backupPyret + " also failed");
    logFailureAndManualFetch(backupPyret, e);
  });

  window.addEventListener("focus", (e) => {
    if(activeEditor) { activeEditor.focus(); }
  });

  function makeEvent() {
    const handlers = [];
    function on(handler) {
      handlers.push(handler);
    }
    function trigger(v) {
      handlers.forEach(h => h(v));
    }
    return [on, trigger];
  }
  let [ onRun, triggerOnRun ] = makeEvent();
  let [ onInteraction, triggerOnInteraction ] = makeEvent();
  let [ onLoad, triggerOnLoad ] = makeEvent();

  programLoaded.fin(function() {
    CPO.editor.focus();
    CPO.editor.cm.setOption("readOnly", false);
  });

  CPO.autoSave = autoSave;
  CPO.save = save;
  CPO.updateName = updateName;
  CPO.showShareContainer = showShareContainer;
  CPO.loadProgram = loadProgram;
  CPO.storageAPI = storageAPI;
  CPO.cycleFocus = cycleFocus;
  CPO.say = say;
  CPO.sayAndForget = sayAndForget;
  CPO.events = {
    onRun,
    triggerOnRun,
    onInteraction,
    triggerOnInteraction,
    onLoad,
    triggerOnLoad
  };

  // We never want interactions to be hidden *when running code*.
  // So hideInteractions should go away as soon as run is clicked
  CPO.events.onRun(() => { document.body.classList.remove("hideInteractions"); });

  let initialState = params["get"]["initialState"];

  window.PYRET_IS_EMBEDDED = false;
  window.PYRET_IN_VSCODE = false;
  if (typeof acquireVsCodeApi === "function") {
    window.MESSAGES = makeEvents({
      CPO: CPO,
      sendPort: acquireVsCodeApi(),
      receivePort: window,
      initialState
    });
    window.PYRET_IS_EMBEDDED = true;
    window.PYRET_IN_VSCODE = true;
  }
  else if((window.parent && (window.parent !== window))) {
    window.MESSAGES = makeEvents({ CPO: CPO, sendPort: window.parent, receivePort: window, initialState });
    window.PYRET_IS_EMBEDDED = true;
  }
});
