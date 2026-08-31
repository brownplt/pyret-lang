const CPO = "https://pyret-horizon.herokuapp.com/editor";

export type State = {
  definitionsAtLastRun: string,
  interactionsSinceLastRun: string[],
  editorContents: string,
  replContents: string,
  messageNumber?: number
}

export type API = {
  sendReset: (state : State) => void,
  currentState: () => State,
  postMessage: (message : any) => void,
  getFrame: () => HTMLIFrameElement,
  setInteractions: (text : string) => void,
  runDefinitions: () => void,
  runInteractionResult: () => Promise<any>,
  onChange: (callback : ((msg : any) => void)) => void,
  clearInteractions: () => void,
};

export type ReadFileOpts = { encoding?: string } | 'utf8';
export type RPCFunctions = {
  fs?: {
    writeFile?: (p: string, buffer: Buffer) => Promise<void>,
    readFile?: (p: string, opts: ReadFileOpts) => Promise<string | Uint8Array>,
    stat?: (p: string) => Promise<{ mtime: number, ctime: number, size: number, native: any }>,
    createDir?: (p: string) => Promise<void>
  },
  path?: {
    join?: (...paths: string[]) => string,
    resolve?: (p: string) => string,
    basename?: (p: string) => string,
    dirname?: (p: string) => string,
    extname?: (p: string) => string,
    relative?: (from: string, to: string) => string,
    'is-absolute'?: (p: string) => boolean
  },
  process?: {
    cwd?: () => string
  }
};

export type EmbedConfig = {
  container: HTMLElement,
  src?: string,
  id?: string,
  state?: State,
  rpc?: RPCFunctions,
  // Which compiler backend the embedded editor loads -- the same knob as
  // code.pyret.org's ?compiler= flag; it is appended to `src` as a query
  // parameter. 'ts' selects the TypeScript port of the compiler; omitting
  // it (or 'pyret') keeps the stock Pyret-hosted compiler. Self-hosted
  // builds have the ts artifacts when built via `npm run build:ts`.
  compiler?: 'pyret' | 'ts',
  options: {
    footerStyle?: 'hide' | 'normal',
    warnOnExit?: boolean,
    hideDefinitions?: boolean,
    hideInteractions?: boolean
  }
};

const defaultOptions = {
  footerStyle: 'hide',
  warnOnExit: false,
  hideDefinitions: false,
  hideInteractions: false
};
const defaultConfig = {
  src: CPO,
  state: false,
  options: defaultOptions
};

type RPCResponse = { resultType: 'value', result: any, } | { resultType: 'exception', exception: any };

function sendRpcResponse(frame : HTMLIFrameElement, data: { callbackId: string }, result: RPCResponse) {
  frame.contentWindow!.postMessage({
    protocol: 'pyret-rpc',
    data: {
      type: 'rpc-response',
      callbackId: data.callbackId,
      ...result
    }
  });
}

async function receiveRPC(frame : HTMLIFrameElement, e: MessageEvent, rpcs: RPCFunctions) : Promise<void> {
  console.log("RPC:", e.data);
  const data = e.data.data;
  const module = (rpcs as any)[data.module];
  if (!module || !(module as any)[data.method]) {
    sendRpcResponse(frame, data, { resultType: 'exception', exception: `Unknown method ${data.method}` });
  }
  else {
    try {
      const result = await (module as any)[data.method](...data.args);
      sendRpcResponse(frame, data, { resultType: 'value', result });
    } catch (exn) {
      sendRpcResponse(frame, data, { resultType: 'exception', exception: String(exn) });
    }
    return;
  }
}

export function makeEmbedConfig(config : EmbedConfig) : Promise<API> {
  let mergedConfig = { ...defaultConfig, ...config };
  let mergedOptions = { ...defaultConfig.options, ...config.options };
  let { container, src } = mergedConfig;
  let id = config.id || ("pyret-embed" + Math.floor(Math.random() * 1000000));
  const hasprop = (obj, prop) => Object.prototype.hasOwnProperty.call(obj, prop);
  console.log("Pyret embed config:", mergedConfig, mergedOptions);
  const propIfTrue = (obj, prop) => { if(obj[prop] === true) { return `&${prop}=true`; } else { return ""; }};
  const propIfPresent = (obj, prop) => { if(hasprop(obj, prop)) { return `&${prop}=${obj[prop]}`; } else { return ""; }};
  const fragment = `${propIfPresent(mergedOptions, "footerStyle")}${propIfPresent(mergedOptions, "warnOnExit")}${propIfTrue(mergedOptions, "hideDefinitions")}${propIfTrue(mergedOptions, "hideInteractions")}`;
  if(mergedConfig.compiler && mergedConfig.compiler !== "pyret") {
    // The compiler choice travels as a query parameter (?compiler=ts), not a
    // hash option, mirroring code.pyret.org's /editor flag; insert it before
    // any hash fragment already present on src.
    const hashAt = src.indexOf("#");
    const base = hashAt === -1 ? src : src.substring(0, hashAt);
    const hash = hashAt === -1 ? "" : src.substring(hashAt);
    src = base + (base.indexOf("?") === -1 ? "?" : "&") + "compiler=" + mergedConfig.compiler + hash;
  }
  if(src.indexOf("#") !== -1) {
    src = src + "&" + fragment;
  }
  else {
    src = src + "#" + fragment;
  }

  let messageNumber = 0;
  let currentState : State;
  function sendReset(frame, state) {
    if(!state) {
      state = {
        definitionsAtLastRun: false,
        interactionsSinceLastRun: [],
        editorContents: "use context starter2024",
        replContents: ""
      };
    }
    if(typeof state === "object") {
      state.messageNumber = 0;
    }
    currentState = state;
    const payload = {
      data: {
        type: 'reset',
        state: typeof state === "string" ? state : JSON.stringify(state)
      },
      protocol: 'pyret'
    };
    frame.contentWindow.postMessage(payload, '*');
  }

  function gainControl(frame : HTMLIFrameElement) {
    frame.contentWindow!.postMessage({
      type: 'gainControl'
    }, '*');
  }

  function setInteractions(frame : HTMLIFrameElement, text : string) {
    messageNumber += 1;
    const change = {
      from: { line: 0, ch: 0 },
      to: { line: 0, ch: 0 },
      text: text
    };
    currentState = { ...currentState, messageNumber, replContents: text };
    const payload = {
      protocol: 'pyret',
      data: {
        type: 'changeRepl',
        change: change
      },
      state: currentState
    };
    frame.contentWindow!.postMessage(payload, '*');
  }

  function runDefinitions(frame : HTMLIFrameElement) {
    messageNumber += 1;
    currentState = { ...currentState, messageNumber, interactionsSinceLastRun: [], definitionsAtLastRun: currentState.editorContents };
    const payload = {
      protocol: 'pyret',
      data: {
        type: 'run'
      },
      state: currentState
    };
    frame.contentWindow!.postMessage(payload, '*');
  }

  function clearInteractions(frame : HTMLIFrameElement) {
    messageNumber += 1;
    const payload = {
      protocol: 'pyret',
      data: {
        type: 'clearInteractions'
      },
      state: currentState
    };
    frame.contentWindow!.postMessage(payload, '*');
  }

  let resultCounter = 0;

  function runInteractionResult(frame : HTMLIFrameElement) {
    const { promise , resolve, reject } = Promise.withResolvers<any>();
    messageNumber += 1;
    const newInteractions = currentState.interactionsSinceLastRun.concat([currentState.replContents])
    currentState = {
        ...currentState,
        messageNumber: messageNumber,
        interactionsSinceLastRun: newInteractions,
        replContents: "",
    };
    const payload = {
      protocol: 'pyret',
      data: {
        type: 'runInteraction',
        reportAnswer: 'interaction' + (++resultCounter)
      },
      state: currentState
    };
    frame.contentWindow!.postMessage(payload, '*');
    window.addEventListener('message', message => {
      if(message.data.protocol !== 'pyret') { return; }
      if(message.source !== frame.contentWindow) { return; }
      const pyretMessage = message.data;
      if(pyretMessage.data.type === 'interactionResult') {
        resolve(pyretMessage.data.textResult);
      }
    });
    return promise;
  }

  function directPostMessage(frame : HTMLIFrameElement, message : any) {
    frame.contentWindow.postMessage(message);
  }

  /* An issue we run into with iframes and scrolling is that CPO wants to scroll
     interactions around sometimes. However, scrolling elements in the iframe
     can scroll the outer page as well to focus it. We don't want that.
     We can prevent this by making the iframe position: fixed, but that makes
     sensible positioning hard. So we create a wrapper iframe that works in the
     normal flow, and make the actual CPO iframe inside that.

     Since CPO only knows how to postMessage to its immediate parent, we also
     proxy all requests through the wrapper, and that's what the client sees.
  */

  const wrapper = document.createElement("iframe");
  wrapper.style = "width: 100%; height: 100%; border: 0; display: block;";

  wrapper.srcdoc = `
<html>
<head>
<style>
html, body { height: 100%; }
body { margin: 0; padding: 0; }
</style>
<script>
window.addEventListener('message', (e) => {
  if (e.source === window.parent) {
    const iframes = document.getElementsByTagName("iframe");
    iframes[0].contentWindow.postMessage(e.data, "*");
  }
  else {
    window.parent.postMessage(e.data, "*");
  }
});
</script>
<body></body>
</html>`;
  container.appendChild(wrapper);
  wrapper.addEventListener("load", () => {
    const wrapperBody = wrapper.contentDocument.body;

    const inner = document.createElement("iframe");
    inner.src = src || CPO;
    inner.style = "width: 100%; height: 100%; border: 0; display: block; position: fixed;";
    inner.width = "100%";
    inner.id = id;
    inner.frameBorder = "0";

    wrapperBody.appendChild(inner);
  });
  const frame = wrapper;
  frame.id = id;

  const { promise, resolve, reject } = Promise.withResolvers<API>();
  setTimeout(() => reject(new Error("Timeout waiting for Pyret to load")), 60000);

  const onChangeCallbacks = [];

  window.addEventListener('message', message => {
    if(message.data.protocol === 'pyret-rpc') {
      receiveRPC(frame, message, mergedConfig.rpc || {}).catch(exn => {
        console.error("Error in RPC handler:", exn);
      });
      return;
    }
    if(message.data.protocol !== 'pyret') {
      return;
    }
    if(message.source !== frame.contentWindow) {
      return;
    }
    const pyretMessage = message.data;
    const typ = pyretMessage.data.type;
    if(typ === 'pyret-init') {
      gainControl(frame);
      if(mergedConfig.state) {
        sendReset(frame, mergedConfig.state);
      }
      const api = makeEmbedAPI(frame);
      (frame as any).pyretEmbed = api;
      resolve(api);
    }
    else if(typ === "changeRepl" || typ === "change") {
      onChangeCallbacks.forEach(cb => cb(pyretMessage));
      currentState = pyretMessage.state;
    }
    else {
      currentState = pyretMessage.state;
    }
  });
  function makeEmbedAPI(frame) {
    return {
      sendReset: (state : State) => sendReset(frame, state),
      postMessage: (message : any) => directPostMessage(frame, message),
      getFrame: () => frame,
      setInteractions: (text : string) => setInteractions(frame, text),
      runDefinitions: () => runDefinitions(frame),
      runInteractionResult: async () => await runInteractionResult(frame),
      onChange: (callback : ((msg : any) => void)) => onChangeCallbacks.push(callback),
      clearInteractions: () => clearInteractions(frame),
      currentState: () => currentState,
    }
  }
  return promise;

}

export function makeEmbed(id : string, container : HTMLElement, src?: string) : Promise<API>{
  const config : EmbedConfig = {
    container,
    id,
    options: { }
  };
  if(src) { config.src = src; }
  return makeEmbedConfig(config);
}
