/*
 * Unit tests for inline-selfcontained.js
 * (run: node --test src/scripts/inline-selfcontained.test.js).
 * These pin the sharp edges: HTML script-data escaping, `$`-safe replacement,
 * mustache-vs-inlined-braces ordering, css url() rebasing, and the fail-loud
 * checks (missing asset, un-inlined reference, sentinel contract).
 */
const test = require("node:test");
const assert = require("node:assert");
const {
  escapeForScript, escapeForStyle, absolutizeCssUrls, buildSelfContained,
  BASE, HASH, URL_FILE_MODE, COMPILER,
} = require("./inline-selfcontained.js");
const Mustache = require("mustache");

test("escapeForScript escapes the bare </script prefix, any case, even unclosed", () => {
  assert.strictEqual(
    escapeForScript("x.js", 'var a = "</script foo><SCRIPT>ok</SCRIPT>";'),
    'var a = "<\\/script foo><SCRIPT>ok<\\/SCRIPT>";');
});

test("escapeForScript rejects <!-- (script-data escaped states) instead of guessing", () => {
  assert.throws(() => escapeForScript("x.js", 'var s = "<!--";'), /x\.js.*<!--/s);
});

test("escapeForStyle escapes </style", () => {
  assert.strictEqual(
    escapeForStyle("x.css", '.a::before { content: "</style>" }'),
    '.a::before { content: "<\\/style>" }');
});

test("absolutizeCssUrls rebases relative urls, leaves data:/http(s):/rooted/#fragment alone", () => {
  const css = [
    "url(../img/a.gif)", "url('b.png?x=1#frag')", 'url("c.woff")',
    "url(data:image/gif;base64,AA==)", "url(https://x.example/d.ttf)",
    "url(/rooted.png)", "url(#gradient)",
  ].join(" ");
  assert.strictEqual(absolutizeCssUrls(css, "D"), [
    "url(D/../img/a.gif)", "url('D/b.png?x=1#frag')", 'url("D/c.woff")',
    "url(data:image/gif;base64,AA==)", "url(https://x.example/d.ttf)",
    "url(/rooted.png)", "url(#gradient)",
  ].join(" "));
});

const TEMPLATE = [
  '<html><head>',
  '<script>window.PYRET = "{{&PYRET}}"; window.PYRET_GZIPPED = "{{ PYRET_GZIPPED }}" === "true";</script>',
  '<script>window.CPO_COMPILER = "{{&CPO_COMPILER}}" || "pyret";</script>',
  '{{^PYRET_GZIPPED}}<link rel="preload" href="{{&PYRET}}" as="script">{{/PYRET_GZIPPED}}',
  '<link rel="stylesheet" href="{{ &BASE_URL }}/css/editor.css" />',
  '<link rel="icon" href="{{ &BASE_URL }}/img/icon.png" />',
  '<script src="{{ &BASE_URL }}/js/shell.js"></script>',
  '</head><body>',
  '<script>var optionalTemplatedHash = "{{ &HASH_OPTIONS }}";</script>',
  '<script>var URL_FILE_MODE = "{{ URL_FILE_MODE }}";</script>',
  '<img src="{{ &BASE_URL }}/img/logo.png">',
  '</body></html>',
].join("\n");

const ASSETS = {
  // `$&` would echo the matched tag under a string-replacement; `{{`/`}}`
  // would be eaten if mustache ran after inlining; `</script x>` must escape.
  "js/shell.js": 'var brace = {{}}; var cash = "$& $1 $`"; var s = "</script x>";',
  "css/editor.css": '.a { background: url(../img/p.gif); } .b { background: url(data:image/gif;base64,AA==); }',
};
const readAsset = (rel) => {
  if (!(rel in ASSETS)) throw new Error("missing asset: " + rel);
  return ASSETS[rel];
};

test("buildSelfContained: end to end on a miniature editor.html", () => {
  const html = buildSelfContained(TEMPLATE, readAsset, Mustache);
  // shell inlined verbatim-modulo-escape; braces and $ intact; tag gone
  assert.ok(html.includes('var brace = {{}}; var cash = "$& $1 $`"; var s = "<\\/script x>";'));
  assert.ok(!html.includes('src="' + BASE + '/js/shell.js"'));
  // css inlined with rebased relative url, untouched data: url
  assert.ok(html.includes("url(" + BASE + "/css/../img/p.gif)"));
  assert.ok(html.includes("url(data:image/gif;base64,AA==)"));
  // gzip flag baked true; the non-gzip preload section dropped
  assert.ok(html.includes('window.PYRET = "' + BASE + '/js/cpo-main.jarr.gz.js"'));
  assert.ok(!html.includes("preload"));
  // ts flavor: nothing bakes its asset URLs (they derive in-page from
  // PYRET's directory); only the backend choice survives, as a sentinel
  assert.ok(!html.includes("PYRET_TS"));
  // runtime sentinels survive for the extension's split/join, exactly once
  assert.strictEqual(html.split(HASH).length - 1, 1);
  assert.strictEqual(html.split(URL_FILE_MODE).length - 1, 1);
  assert.strictEqual(html.split(COMPILER).length - 1, 1);
  // non-css/js BASE references (img, icon) intentionally survive
  assert.ok(html.includes('src="' + BASE + '/img/logo.png"'));
});

test("buildSelfContained: missing asset is a build error, not a fallback", () => {
  const t = TEMPLATE.replace("js/shell.js", "js/nope.js");
  assert.throws(() => buildSelfContained(t, readAsset, Mustache), /missing asset: js\/nope\.js/);
});

test("buildSelfContained: a BASE script the pattern can't inline is a build error", () => {
  // single-quoted src doesn't match the inline pattern; the leftover check
  // must catch it rather than shipping a MIME-doomed reference
  const t = TEMPLATE.replace(
    '<script src="{{ &BASE_URL }}/js/shell.js"></script>',
    "<script src='{{ &BASE_URL }}/js/shell.js'></script>");
  assert.throws(() => buildSelfContained(t, readAsset, Mustache), /survived inlining/);
});

test("buildSelfContained: losing a runtime sentinel is a build error", () => {
  const t = TEMPLATE.replace('{{ &HASH_OPTIONS }}', '');
  assert.throws(() => buildSelfContained(t, readAsset, Mustache), /sentinel contract/);
});

// The same asset files are also served UN-inlined by the normal server, where
// the extension's placeholder fill never touches them -- so an asset that
// contains a sentinel must be rejected, not inlined with divergent meaning.
test("buildSelfContained: an asset containing a webview sentinel is a build error", () => {
  const assets = { ...ASSETS, "js/shell.js": 'var u = "__PYRET_WEBVIEW_HASH__";' };
  assert.throws(
    () => buildSelfContained(TEMPLATE, (rel) => assets[rel], Mustache),
    /js\/shell\.js contains the sentinel prefix/);
});

test("checkInlinable: only the sentinel prefix is rejected; braces and tags are fine", () => {
  const { checkInlinable } = require("./inline-selfcontained.js");
  // Assets are inlined AFTER the template render, so mustache-looking text in
  // them (other engines' tags, minified `){{var`) is inert and accepted.
  checkInlinable("js/x.js", 'f(a){{var t = "{{SOME_OTHER_TAG}} {{ &BASE_URL }}";}}');
});
