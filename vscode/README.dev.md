Borrows heavily from https://github.com/microsoft/vscode-extension-samples/tree/main/custom-editor-sample

To run, first you must symlink `build` to a the `build/` directory of
`code.pyret.org`. You can get one by cloning `code.pyret.org` elsewhere and
symlinking to it.

Then:

```
npm i
npm run compile
npx vscode-test-web --browserType=chromium --extensionDevelopmentPath . ./sampleFiles/
```

User settings for avoiding diff views using the fancy editor; put in
`.vscode/settings.json` (or set via the menu):

```
{
    "workbench.editorAssociations": {
        "{git}:/**/*.{arr}": "default"
    }
}
```

(Courtesy of https://github.com/microsoft/vscode-discussions/discussions/799)

## Compiler backend

The webview can boot the editor on either the stock Pyret-hosted compiler or
the TypeScript port (the same knob as code.pyret.org's `?compiler=ts` flag)
via the `pyret-parley.compiler` setting (`"pyret"` (default) or `"ts"`).
The ts flavor requires the `code.pyret.org` build you symlinked to have run
`make web-ts` (producing `cpo-main-ts.jarr.gz.js` and `ts-compiler.gz.js`)
before `npm run compile` copies assets into `dist/`; the default setting and
build are unchanged.

Grammar and language-configuration contributed by Seth Poulsen.
