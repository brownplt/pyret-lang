## Installation and Use

- Copy `node_modules/pyret-embed/dist/` to where you serve static files
- Note that all the Pyret stuff is set up to correctly work with relative paths
internally. You just need to make sure the paths to pyret.js and
editor.embed.html work from your source.
- From your website:
- ```
      <script type="module" src="/dist/pyret.js"></script>
      <div id="example1" class="embed-container"></div>
      <script type="module">
  async function example1() {
        const iframeContainer = document.getElementById("example1");
        const embed = await makeEmbed('basic1', iframeContainer, "/dist/build/web/editor.embed.html#hideFooter=true");

        embed.sendReset({
          definitionsAtLastRun: "use context starter2024\n\n'Hello!'",
          interactionsSinceLastRun: [],
          editorContents: "use context starter2024\n\n'Hello!'",
          replContents: ""
        });
      }
      example1();
      </script>
   ```
See examples/ for more examples!

## Choosing the compiler backend

The embedded editor can run on either the stock Pyret-hosted compiler or the
TypeScript port of the compiler (`pyret-lang/src/ts-compiler`) — the same
opt-in as code.pyret.org's `?compiler=ts` flag. Pass `compiler: 'ts'` in the
config form of the API and the flag is appended to the embed URL for you:

```
const embed = await makeEmbedConfig({
  container,
  src: "/dist/build/web/editor.embed.html",
  compiler: "ts",   // default: "pyret" (the stock compiler)
  options: { footerStyle: "hide" },
});
```

For self-hosted builds the ts artifacts (`cpo-main-ts.jarr.js`,
`ts-compiler.js`) are only present if you build with `npm run build:ts`
(which runs the regular build plus code.pyret.org's `make web-ts`); the
default `npm run build` produces the stock-only bundle and the default
behavior is unchanged.

## API

```
type State = {
    definitionsAtLastRun: string;
    interactionsSinceLastRun: string[];
    editorContents: string;
    replContents: string;
    messageNumber?: number;
};
type API = {
    sendReset: (state: State) => void;
    postMessage: (message: any) => void;
    getFrame: () => HTMLIFrameElement;
    setInteractions: (text: string) => void;
    runDefinitions: () => void;
    runInteractionResult: () => Promise<any>;
    onChange: (callback: ((msg: any) => void)) => void;
    clearInteractions: () => void;
    currentState: () => State;
};
declare function makeEmbed(id: string, container: HTMLElement, src?: string): Promise<API>;
```

## Running Examples in This Repo

To see the examples in this repository:

```
$ git clone ...
$ npm install
$ python3 -m http.server # or your favorite static server
```

Then open [localhost:8000/examples/basic.html](http://localhost:8000/src/basic.html) in browser.
