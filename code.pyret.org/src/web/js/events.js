let messageCounter = 0;
let targetOrigin = POSTMESSAGE_ORIGIN;
let RECEIVED_RESET = false;
function commSetup(config, messageCallback, gainControl, loseControl) {
  const callbacks = {};
  let callbackCounter = 0;

  function sendRpc(data, resolve, reject) {
    callbackCounter += 1;
    const callbackId = `${callbackCounter}-${data.module}-${data.method}`
    callbacks[callbackId] = [ resolve, reject ];
    config.sendPort.postMessage({
      protocol: "pyret-rpc",
      data: { ...data, callbackId }
    }, '*');
  }

  // state is optional; if not provided we use getCurrentState to fill it
  //   we make it optional because sometimes getCurrentState() is a little racy
  //   for example onInteraction triggers before the interaction is complete and
  //   can still see the just-run repl contents instead of empty repl contents
  function sendEvent(data, description, state) {
    if(!RECEIVED_RESET && data.type !== "pyret-init") {
      console.log("Pyret skipping a message synthesized before initialization via 'reset':", data, getCurrentState(config));
      return;
    }
    messageCounter += 1;
    if(!state) {
      state = getCurrentState(config)
    }
    state.messageNumber = messageCounter;
    console.log("Sending from CPO ", data, state);
    config.sendPort.postMessage(
      {
        protocol: "pyret",
        timestamp: window.performance.now(),
        data: data,
        state,
        description
      },
      targetOrigin
    );
  }


  config.receivePort.onmessage = function (event) {
    if (
      typeof event.data === "string" &&
      event.data.indexOf("setImmediate") === 0
    ) {
      return;
    }
    if (event.data.type === "loseControl") {
      loseControl(config);
      return;
    }
    else if (event.data.type === "gainControl") {
      gainControl(config);
      return;
    }
    if (event.data.protocol === "pyret-rpc") {
      if(!callbacks[event.data.data.callbackId]) {
        console.error("No callback for ", event.data.data.callbackId, event, callbacks);
        return;
      }
      else {
        const [resolve, reject] = callbacks[event.data.data.callbackId];
        delete callbacks[event.data.data.callbackId];
        if(event.data.data.resultType === "exception") {
          reject(event.data.data.exception); 
        }
        else if(event.data.data.resultType === "value") {
          resolve(event.data.data.result);
        }
        else {
          const message = "Internal RPC error, unknown RPC response event (no result or exception)";
          console.error(message, event);
          reject({ event, message });
        }
      }
      return;
    }
    if (event.data.protocol !== "pyret") {
      return;
    }
    messageCallback(event.data.data, event.data.state);
  };
  return { sendEvent, sendRpc };
}

/**
 * Every message goes on the queue *except* for run.
 * 
 * Run empties the queue, stops the program, sets the current state, and runs the program
 * 
 */
let messageQueue = [];

let interactionsSinceLastRun = [];
// Sometimes there are interleavings of edits with running, so the currently-shown
// definitions isn't the one used for running the program.
// `false` indicates that the program has not been run yet
let definitionsAtLastRun = false;
function getCurrentState(config) {
  return {
    editorContents: config.CPO.editor.cm.getValue(),
    definitionsAtLastRun,
    interactionsSinceLastRun,
    replContents: config.replCM().getValue()
  };
}

function makeEvents(config) {
  const editor = config.CPO.editor;
  const replCM = function() { return config.CPO.replWidget.cm; }
  config.replCM = replCM;

  config.inControl = false;

  var runButton = $("#runButton");
  var runDropdown = $("#runDropdown");
  var breakButton = $("#breakButton");
  function sendControlWarning() {
    comm.sendEvent({
      type: "illegal-action"
    }, "Tried to interact while not in control.");
  }
  function loseControl(config) {
    config.inControl = false;
    config.CPO.editor.cm.setOption('readOnly', 'nocursor');
    config.CPO.replWidget.cm.setOption('readOnly', 'nocursor');
    config.CPO.editor.cm.getWrapperElement().addEventListener('click', sendControlWarning);
    config.replCM().getWrapperElement().addEventListener("click", sendControlWarning);
    runButton.hide();
    runDropdown.hide();
    breakButton.hide();
  }
  function gainControl(config) {
    config.inControl = true;
    config.CPO.editor.cm.setOption('readOnly', false);
    config.CPO.replWidget.cm.setOption('readOnly', false);
    config.CPO.editor.cm.getWrapperElement().removeEventListener('click', sendControlWarning);
    config.replCM().getWrapperElement().removeEventListener("click", sendControlWarning);

    runButton.show();
    runDropdown.show();
    breakButton.show();
  }
  
  async function reset(state) {
    interactionsSinceLastRun = [];
    messageCounter = state.messageNumber;
    definitionsAtLastRun = state.definitionsAtLastRun;
    if(typeof state.definitionsAtLastRun === 'string') {
      editorUpdate(state.definitionsAtLastRun);
      await window.RUN_CODE(state.definitionsAtLastRun);
    }
    else if (state.definitionsAtLastRun === false) {
      // "Never run": the empty run below establishes the REPL baseline. Two
      // audiences depend on it -- interaction replay (the loop after this
      // branch assumes a runtime to replay against), and hosts that show the
      // interactions pane, whose prompt only appears at a run's completion
      // (repl-ui's showPrompt) -- pyret-embed's default reset state relies on
      // that for a live prompt at boot.
      //
      // A host that needs neither can send warmStart: false to skip it.
      // While this run is in flight the editor LOOKS ready, but a Run click
      // is silently swallowed and edits race the contents install below --
      // so a host whose pane hides interactions (the vscode cpo editor) pays
      // boot latency plus a race window for an invisible effect. Interaction
      // replay always gets its baseline regardless of the flag.
      if (state.warmStart !== false || (state.interactionsSinceLastRun || []).length > 0) {
        await window.RUN_CODE("");
      }
    }
    const interactions = state.interactionsSinceLastRun;
    for(let i = 0; i < interactions.length; i += 1) {
      await runInteraction(interactions[i]);
    }
    editorUpdate(state.editorContents);
    replUpdate(state.replContents);
    // The host-fed settle point (see EXPECTS_HOST_RESET in beforePyret.js):
    // everything above -- the awaited warm-start run included -- is done and
    // the host's contents are installed. RECEIVED_RESET (commSetup) marks
    // receipt, which is the START of that window; this marks its end.
    window.EDITOR_CONTENTS_SETTLED = true;
  }

  config.CPO.events.onLoad(async function () {
    replCM().on("change", function (instance, change) {
      if (change.origin === thisAPI || change.origin === "setValue") {
        return;
      }
      comm.sendEvent({
        type: "changeRepl",
        change: change,
      }, "Edited the last interaction.");
    });
    comm.sendEvent({
      type: "pyret-init"
    });
    if(config.initialState) {
      addMessage({
        process: async () => {
          const result = await resetMessage({ state: config.initialState }, config.initialState);
          gainControl(config);
          return result;
        }
      });
    }

  });

  const comm = commSetup(config, onmessage, gainControl, loseControl);

  function editorUpdate(newCode) {
    editor.cm.replaceRange(
      newCode,
      { line: 0, ch: 0},
      { line: editor.cm.lastLine(), ch: 99999 },
      thisAPI
    );
    editor.cm.refresh();
  }

  function replUpdate(newCode) {
    replCM().replaceRange(
      newCode,
      { line: 0, ch: 0},
      { line: replCM().lastLine(), ch: 99999 },
      thisAPI
    );
    window.setTimeout(() => {
      replCM().refresh();
      if(config.inControl) {
        replCM().focus();
      }
    }, 100);
  }

  // Thanks internet! https://github.com/codemirror/CodeMirror/issues/3691
  const thisAPI = "@ignore-this-api";

  editor.cm.on("change", function (instance, change) {
    if (change.origin === thisAPI || change.origin === "setValue") {
      return;
    }
    comm.sendEvent({
      type: "change",
      change: change,
    }, "Made a change to the program.");
  });

  config.CPO.events.onRun(function () {
    interactionsSinceLastRun = [];
    definitionsAtLastRun = getCurrentState(config).editorContents;
    comm.sendEvent({
      type: "run"
    }, "Ran the program.");
  });


  async function runProgram(state) {
    interactionsSinceLastRun = [];
    const code = state.editorContents
    editorUpdate(code);
    definitionsAtLastRun = code;
    await window.RUN_CODE(code);
    if(!config.inControl) {
      replCM().display.input.blur();
      replCM().setOption("readOnly", "noCursor");
    }
  }

  config.CPO.events.onInteraction(function (interaction) {
    interactionsSinceLastRun.push(interaction);
    // When we get this event, the repl may or may not have been cleared,
    // so we explicitly pass it as the empty string.
    const state = { ...getCurrentState(config), replContents: "" };
    comm.sendEvent({
      type: "runInteraction"
    }, `Ran the last interaction, ${interaction}.`, state);
  });

  async function clearInteractions(state) {
    interactionsSinceLastRun = [];
    $("#output").empty();
  }

  async function runInteraction(src, reportAnswer) {
    // Because of a bug we introduced, a bunch of rooms ended up with states with null
    // interactions. If left null, they will put the editor into an unrecoverable state.
    // So, we simply run them as empty interactions, and rely on future
    // runs/resets making things become eventually consistent
    if(src === null) { src = ""; }
    interactionsSinceLastRun.push(src);
    if(!config.inControl) {
      $(".repl-prompt")
        .find(".CodeMirror")[0]
        .CodeMirror.setOption("readOnly", "nocursor");
    }
    await window.RUN_INTERACTION(src);

    // We do this again because the CPO infrastructure
    // explicitly re-enables the prompt at the end of
    // RUN_INTERACTION, but we don't want that here!
    if(!config.inControl) {
      $(".repl-prompt")
        .find(".CodeMirror")[0]
        .CodeMirror.setOption("readOnly", "nocursor");
      replCM().display.input.blur();
    }

    if(typeof reportAnswer === 'string') {
      const state = { ...getCurrentState(config), replContents: "" };
      const elts = $("#output").children(".echo-container").last().nextAll();
      const texts = elts.map((_, e) => $(e).text()).get();
      const htmls = elts.map((_, e) => $(e).html()).get();
      const textResult = JSON.stringify({ texts, htmls });
      comm.sendEvent({
        type: "interactionResult",
        reportAnswer,
        textResult,
      }, `Reporting an interaction by request, ${src} ${reportAnswer}.`, state);
    }
  }

  const initialState = {
    editorContents: "use context starter2024\n\n",
    interactionsSinceLastRun: [],
    definitionsAtLastRun: "use context starter2024\n\n",
    replContents: ""
  };

  function resetFromShare(link) {
    var initialParams = url.parse(link); // https://code.pyret.org/editor#share=2390485
    // initialParams = "share=2390485"
    var params = url.parse("/?" + initialParams["hash"]);
    if(params["get"]["share"]) {
      editorUpdate("");
      definitionsAtLastRun = "";
      interactionsSinceLastRun = [];
      const ran = window.RUN_CODE("");
      const toLoad = ran.then(() => {
        return CPO.storageAPI.then((api) => {
          return api.getSharedFileById(params["get"]["share"]);
        });
      });
      CPO.loadProgram(toLoad).then((text) => {
        editorUpdate(text);
        // The share-link flavor of a host reset settles here (it bypasses
        // reset(), so the end of reset() can't cover it).
        window.EDITOR_CONTENTS_SETTLED = true;
      })
      .catch((e) => {
        console.error("Error loading initial state from share link: ", e, link);
      });
    }
  }

  async function resetMessage(message, state) {
    console.log("Got explicit reset: ", message, "current state", getCurrentState(config));
    RECEIVED_RESET = true;
    
    if(message.state === "") {      
      return reset(initialState);
    }
    // This means we got a CPO link as the initial state.
    if((typeof APP_BASE_URL === 'string' && APP_BASE_URL !== "" && message.state.startsWith(APP_BASE_URL)) || message.state.startsWith("https://" + window.APP_DOMAIN)) {
      return resetFromShare(message.state);
    }
    try {
      const state = JSON.parse(message.state);
      return reset(state);
    }
    catch(e) {
      console.error("Parse error in initial state, using default initial state");
      return reset(initialState);
    }
  }

  function stop() {
    try {
      config.CPO.replWidget.stop();
    }
    catch(e) {
      console.log("runProgram caught as expected? ", e);
    }    

  }

  let pending = false;
  function nextMessage() {
    if(messageQueue.length === 0 ) { return; }
    if(!pending) {
      const { process } = messageQueue.pop();
      pending = process();
      pending.finally(() => {
        pending = false;
        nextMessage();
      });
    }
  }
  function addMessage(message) {
    messageQueue.unshift(message);
    nextMessage();
  }
  function onmessage(message, state) {
    if(message.type === "reset") {
      messageQueue = [];
      stop();
      addMessage({
        process: async () => { return resetMessage(message, state); }
      });
      return;
    }
    if(!config.inControl && (state.messageNumber !== messageCounter + 1)) {
      console.log("Messages received in a strange order: ", message, state, messageCounter, getCurrentState(config));
      messageQueue = [];
      stop();
      addMessage({
        process: async () => { return reset(state); }
      });
      return;
    }
    else {
      messageCounter += 1;
    }


    switch (message.type) {
      // Run events are special; they re-synchronize all clients and empty the pending queue.
      // This makes it so clicking “run” will bring everyone back together, terminate their games, etc.
      // Other cases should never reset the messageQueue
      case "run":
        messageQueue = [];
        stop();
        addMessage({ process: async () => { return runProgram(state); } });
        break;
      case "clearInteractions":
        addMessage({ process: async () => { return clearInteractions(state); } });
        break;
      case "setContents":
        addMessage({ process: async () => { return editorUpdate(message.text); } });
        break;
      case "change":
        addMessage({
          process: async () => {
            editor.cm.replaceRange(
              message.change.text,
              message.change.from,
              message.change.to,
              thisAPI
            );
            if(!config.inControl && config.CPO.editor.cm.getValue() !== state.editorContents) {
              console.log("Editor contents disagreed with message state, synchronizing.", config.CPO.editor.cm.getValue(), state.editorContents)
              editorUpdate(state.editorContents);
            }
          }
        })
        break;
      case "changeRepl":
        addMessage({
          process: async () => {
            replCM().replaceRange(
              message.change.text,
              message.change.from,
              message.change.to,
              thisAPI
            );
          }
        });
        // These need to be separate messages to ensure that replCM() processes
        // before asking it to refresh
        addMessage({
          process: async () => {
            replCM().refresh();
            if(config.inControl) {
              replCM().focus();
            }
          }
        });
        addMessage({
          process: async() => {
            if(!config.inControl && replCM().getValue() !== state.replContents) {
              console.log("REPL contents disagreed with message state, synchronizing.", replCM().getValue(), state.replContents)
              replUpdate(state.replContents);
            }
          }
        });
        break;
      case "runInteraction":
        addMessage({
          process: async () => {
            const interactions = state.interactionsSinceLastRun;
            const src = interactions[interactions.length - 1];
            return runInteraction(src, message.reportAnswer);
          }
        });
        break;
    }
  }

  return {
    sendRpc: (module, method, args) => {
      const { promise, resolve, reject } = Promise.withResolvers();
      const data = { module, method, args }
      comm.sendRpc(data, resolve, reject);
      return promise;
    }
  }
}
