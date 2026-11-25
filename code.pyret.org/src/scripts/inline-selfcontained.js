/*
 * inline-selfcontained.js
 *
 * Emits a (mostly) self-contained variant of editor.html, and sets up for a
 * loading trick in beforePyret.js for the gzipped compiler/runtime bundle.
 *
 * The self-contained page has a few uses:
 *
 * - Fewer network requests
 * - We've observed hosts that serve the editor's assets WITHOUT an executable
 *   MIME type (text/plain + nosniff, so <script src>/<link rel=stylesheet> are
 *   refused execution) -- most notably GitLab's Web IDE.
 *
 * The gzipped bundle is not inlined; rather the bundle is read in via fetch and
 * decompressed, then shoved in a script tag "manually" (detected via the new
 * PYRET_GZIPPED variable). Rationale:
 *
 * - We really shouldn't have been serving it uncompressed in the first place
 * - Inlining without compressing is huge; inlining with compression is weird
 *   (base64 encode, then decode?)
 * - We still need a way to execute it in the webview, so raw bytes ->
 *   decompress -> script tag it is
 *
 * For real reasons, the resulting "self-contained" page is not *quite*
 * self-contained. In VScode extensions, we *cannot know our BASE_URL at build
 * time*; the (3rd party to us) editor itself supplies it at runtime. Further,
 * we have some configuration options that we want to be able to
 * programmatically set on hash-parameters of an embedded URL. So we leave a
 * residue of clear `__PYRET_` variables to be filled in later. That means the
 * instantiation looks like:
 *
 * 1. Replace build-time-fixed mustache variables using a hardcoded dictionary
 *    in this script (a subset of the things in .env for the server use case).
 *    The runtime-dynamic values are replaced with special __PYRET_ variables.
 * 2. Locate JS/CSS import tags and inline the corresponding JS and CSS files
 *
 * The order matters (mustache can trip on JS-isms like {{ and }}), so we must
 * inline after substituting. Later steps (right now, just the webview) do not
 * use or rely on the {{ }} syntax, and just do a bare string replace on the
 * special residue __PYRET_ variables.
 *
 * We try to fail loudly here, so missing files, provably bad markup, etc are
 * build errors.
 *
 * Cross-file dependencies:
 * - pyretCPOWebEditor.ts: refers to the __PYRET_ sentinels, and fills them in at runtime
 * - beforePyret.js: detects the PYRET_GZIPPED variable and decompresses the bundle
 */
const fs = require("fs");
const path = require("path");

const BASE = "__PYRET_WEBVIEW_BASE_URL__";
const HASH = "__PYRET_WEBVIEW_HASH__";
const URL_FILE_MODE = "__PYRET_WEBVIEW_URL_FILE_MODE__";
const COMPILER = "__PYRET_WEBVIEW_COMPILER__";
const SENTINEL_PREFIX = "__PYRET_WEBVIEW_";

// The complete render dictionary for the self-contained template:
// runtime-dynamic values -> literal sentinels; the self-contained constants
// are baked; every other (server-only) var renders to "" as usual.
// TODO(joe): set things up so this can be a *comprehensive* hardcoded list, or
// come from a .env.selfcontained or similar. Right now this script is a bit
// bespoke so having it inline here makes sense.
const TEMPLATE_VARS = {
  BASE_URL: BASE,
  PYRET: BASE + "/js/cpo-main.jarr.gz.js",
  PYRET_GZIPPED: "true",
  // The TS-compiler flavor (editor.html's CPO_COMPILER script). Its asset
  // URLs are derived in-page from PYRET's directory + canonical names, so
  // baking PYRET above covers them; only the choice between backends is a
  // runtime sentinel (known at webview startup, from the
  // pyret-parley.compiler setting). The gz ts assets only need to exist in
  // the build (`make web-ts`) when a host actually selects ts.
  CPO_COMPILER: COMPILER,
  HASH_OPTIONS: HASH,
  URL_FILE_MODE: URL_FILE_MODE,
  IMAGE_PROXY_BYPASS: "true",
};

/*
 * Explicitly call it an error if we end up trying to reference the __PYRET_
 * variables before we put them in ourselves. This script should own all those
 * insertions.
 */
function checkInlinable(rel, content) {
  if (content.includes(SENTINEL_PREFIX)) {
    throw new Error(
      "inline-selfcontained: " + rel + " contains the sentinel prefix `" +
      SENTINEL_PREFIX + "`, which the vscode extension's placeholder fill " +
      "would rewrite inside the inlined copy at webview startup (and would " +
      "be left alone when the same file is served un-inlined by the server)."
    );
  }
}

/*
 * Inlined content must not contain anything the HTML parser would read as
 * markup while in the script-data / style-data states:
 *
 *  - `</script` (any case, in any position) ends the script element, even
 *    mid-string. (HTML 4.01 ended script data at ANY `</`+letter, the ETAGO
 *    delimiter -- w3.org/TR/html401/appendix/notes.html#notes-specifying-data,
 *    B.3.2; the HTML5 tokenizer narrows that to `</script`.) *Escaping* it, not
 *    disallowing it, because it is entirely plausible that we (or a library we
 *    use...) reasonably injects script tags, given our eval-based runtime.
 *    `<\/script` is the traditional fix and is meaning-preserving anywhere.
 *  - `<!--` flips the parser into the "script data escaped" states, where,
 *    among other things, `</script>` has a different meaning. We shouldn't do
 *    this, and should be skeptical of JS files that do, so fail at build time.
 *  Both rules, and the suggested escapes, are spelled out in
 *  html.spec.whatwg.org/multipage/scripting.html#restrictions-for-contents-of-script-elements
 *
 * Notably, other libraries make mistakes on this (this is a "trust
 * Joe" comment, he reviewed them), or boil down to more-or-less these
 * regexes. Rather than adding npm dependencies, just write it.
 */
function escapeForScript(rel, s) {
  if (/<!--/.test(s)) {
    throw new Error(
      "inline-selfcontained: " + rel + " contains `<!--`, which changes how " +
      "the HTML parser matches </script> once inlined (script-data escaped " +
      "states). Decide how to escape it for this asset; see the comment above " +
      "escapeForScript."
    );
  }
  return s.replace(/<\/(script)/gi, "<\\/$1");
}
function escapeForStyle(rel, s) { return s.replace(/<\/(style)/gi, "<\\/$1"); }

// A stylesheet inlined into a <style> in the top-level document would resolve
// its relative url(...)s against the page instead of against .../css/.
function absolutizeCssUrls(css, cssDirUrl) {
  return css.replace(/url\(\s*(['"]?)([^'")]+)\1\s*\)/gi, (m, q, ref) => {
    const r = ref.trim();
    if (/^(data:|https?:|\/|#)/i.test(r)) return m;
    return "url(" + q + cssDirUrl + "/" + r + q + ")";
  });
}

function countOccurrences(haystack, needle) {
  return haystack.split(needle).length - 1;
}

/*
 * The whole transform, on strings: mustache-render the clean template, then
 * inline every BASE-sentinel js/css reference via readAsset(rel), then check
 * the result. Separated from main() so it can be unit-tested.
 */
function buildSelfContained(template, readAsset, Mustache) {
  // 1. Render TEMPLATE_VARS on the CLEAN template (mustache sees only its
  //    intended input). The {{^PYRET_GZIPPED}} preload section drops out here.
  let html = Mustache.render(template, TEMPLATE_VARS);

  // 2. Inline the JS files (their src now starts with the BASE sentinel).
  //    Function replacers -- the library code is full of `$`, which a string
  //    replacement would treat as `$&`/`$1`/`$'`. checkInlinable runs on the
  //    raw bytes, before any of our own rewrites introduce sentinels.
  html = html.replace(
    new RegExp('<script\\b[^>]*\\bsrc="' + BASE + '/js/([^"]+)"[^>]*>\\s*</script>', "gi"),
    (tag, rel) => {
      const code = readAsset("js/" + rel);
      checkInlinable("js/" + rel, code);
      return "<script>\n" + escapeForScript("js/" + rel, code) + "\n</script>";
    }
  );

  // 3. Inline the stylesheets (absolutizing their url()s to the BASE sentinel).
  html = html.replace(
    new RegExp('<link\\b[^>]*\\bhref="' + BASE + '/css/([^"]+)"[^>]*>', "gi"),
    (tag, rel) => {
      const css = readAsset("css/" + rel);
      checkInlinable("css/" + rel, css);
      const dirUrl = path.posix.dirname(BASE + "/css/" + rel);
      return "<style>\n" + escapeForStyle("css/" + rel, absolutizeCssUrls(css, dirUrl)) + "\n</style>";
    }
  );

  // 4. No MIME-blocked load may still point at the sentinel base. nosniff only
  //    refuses script/style destinations, so the window.PYRET url (fetched, not
  //    a script load) and img/icon references survive intentionally; the check
  //    is: no <script src> at BASE at all, and no <link> at a BASE .css.
  //    Catches template drift the inline patterns above would silently skip
  //    (single-quoted attrs, a new asset directory, ...).
  const leftoverScripts = html.match(
    new RegExp("<script\\b[^>]*\\bsrc\\s*=\\s*['\"]?" + BASE + "[^>]*>", "gi"));
  const leftoverStyles = html.match(
    new RegExp("<link\\b[^>]*\\bhref\\s*=\\s*['\"]?" + BASE + "[^'\"]*\\.css[^>]*>", "gi"));
  if (leftoverScripts || leftoverStyles) {
    throw new Error(
      "inline-selfcontained: reference(s) to the webview base survived inlining " +
      "(the tag didn't match the inline patterns -- quoting? new directory?):\n  " +
      [...(leftoverScripts || []), ...(leftoverStyles || [])].join("\n  ")
    );
  }

  // 5. The extension fills exactly these sentinels (split/join); if the
  //    template stops using one, or uses one twice where once is assumed, that
  //    contract broke -- here, not in the webview.
  const hashCount = countOccurrences(html, HASH);
  const modeCount = countOccurrences(html, URL_FILE_MODE);
  const compilerCount = countOccurrences(html, COMPILER);
  if (hashCount !== 1 || modeCount !== 1 || compilerCount !== 1 || countOccurrences(html, BASE) < 1) {
    throw new Error(
      "inline-selfcontained: sentinel contract broke: expected HASH x1 (got " +
      hashCount + "), URL_FILE_MODE x1 (got " + modeCount + "), COMPILER x1 (got " +
      compilerCount + "), BASE >= 1"
    );
  }

  return html;
}

function main() {
  const [editorPath, assetRoot, outPath] = process.argv.slice(2);
  if (!editorPath || !assetRoot || !outPath) {
    console.error("usage: node inline-selfcontained.js <editor.html> <asset-root> <out.html>");
    process.exit(2);
  }
  const readAsset = (rel) => {
    const p = path.join(assetRoot, rel);
    try {
      return fs.readFileSync(p, "utf8");
    } catch (e) {
      throw new Error(
        "inline-selfcontained: editor.html references an asset that is not in " +
        "the build: " + p + " (a self-contained template with a dead reference " +
        "would only fail later, inside the webview, on Open VSX-backed hosts)"
      );
    }
  };
  const html = buildSelfContained(
    fs.readFileSync(editorPath, "utf8"), readAsset, require("mustache"));
  fs.writeFileSync(outPath, html);
  console.log("wrote self-contained editor template: " + outPath);
}

module.exports = { escapeForScript, escapeForStyle, absolutizeCssUrls, checkInlinable, buildSelfContained, BASE, HASH, URL_FILE_MODE, COMPILER };

if (require.main === module) main();
