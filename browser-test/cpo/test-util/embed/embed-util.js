function sendReset(frame, state) {
  if(!state) {
    state = {
      definitionsAtLastRun: false,
      interactionsSinceLastRun: [],
      editorContents: "use context starter2024",
      replContents: ""
    };
  }
  if(typeof state !== 'string') {
    state = JSON.stringify(state);
  }
  frame.contentWindow.postMessage({
    data: {
      type: 'reset',
      state
    },
    protocol: 'pyret'
  });
}
function directPostMessage(frame, message) {
  frame.contentWindow.postMessage(message);
}

function makeEmbedAPI(frame) {
  return {
    sendReset: (state) => sendReset(frame, state),
    postMessage: (message) => directPostMessage(frame, message)
  }
}
