const assert = require("node:assert/strict");
const { test } = require("node:test");
const { IDLE, install, outputHas, waitOrFail } = require("./helpers");

const PROGRAM = `use context starter2024

fun double(n :: Number) -> Number:
  n + n
end

check:
  double(3) is 6
end
`;

async function runAndCheck(page, action = "window.PA.run()") {
  await page.eval("window.PA.clearOutput()");
  await page.eval(action);
  await waitOrFail(page, `window.PA.doneRendering() && ${IDLE}`, 60000, "Run did not finish");
  assert.equal((await page.eval("window.PA.shipshapeResult()")).shipshape, true);
}

module.exports = function registerFolding(getSession) {
  test("Run keeps a function folded and clears its old highlight", { timeout: 180000 }, async () => {
    const page = getSession().page;
    await install(page, PROGRAM);
    await page.eval(`(() => {
      const cm = window.CPO.editor.cm;
      cm.setCursor(cm.lastLine(), 0);
      cm.foldCode(2, null, "fold");
    })()`);
    const body = "{line: 3, ch: 3}";
    const folded = `!!window.CPO.editor.cm.isFolded(${body})`;
    const highlighted = `window.CPO.editor.cm.findMarksAt(${body})
      .some(mark => mark.className === "previous-run-highlight")`;
    for (let run = 1; run <= 2; run++) {
      // The old highlight overlaps the fold; Run must only remove the highlight.
      await page.eval(`void window.CPO.editor.cm.markText(
        {line: 3, ch: 2}, {line: 3, ch: 7}, {className: "previous-run-highlight"})`);
      assert.equal(await page.eval(folded), true, "the function must start folded");
      assert.equal(await page.eval(highlighted), true, "the highlight must exist before Run");
      await runAndCheck(page, "document.getElementById('runButton').click()");
      assert.equal(await page.eval(folded), true, "Run unfolded the function");
      assert.equal(await page.eval(highlighted), false, "Run left the old highlight in place");

      if (run === 1) {
        await page.eval("window.CPO.editor.cm.foldCode(2, null, 'unfold')");
        assert.equal(await page.eval(folded), false, "the function must still unfold normally");
        await page.eval("window.CPO.editor.cm.foldCode(2, null, 'fold')");
      }
    }
  });

  test("Run preserves the context marker and a context change replaces it normally",
    { timeout: 180000 }, async () => {
      const page = getSession().page;
      await install(page, PROGRAM);
      await page.eval(`(() => {
        window.runMarkerTest = {
          context: window.CPO.editor.cm.getAllMarks().find(mark => mark.className === "useline")
        };
      })()`);
      assert.equal(await page.eval("!!window.runMarkerTest.context"), true,
        "the context line must have a marker before Run");
      await runAndCheck(page);
      assert.equal(await page.eval("!!window.runMarkerTest.context.find()"), true,
        "Run cleared the context marker");

      await page.eval(`(() => {
        window.CPO.editor.setContextLine("use context essentials2024\\n");
        window.runMarkerTest.replacement = window.CPO.editor.cm.getAllMarks()
          .find(mark => mark.className === "useline");
      })()`);
      assert.equal(await page.eval("!!window.runMarkerTest.context.find()"), false,
        "changing contexts must clear the old marker");
      assert.equal(await page.eval(`!!window.runMarkerTest.replacement &&
        window.runMarkerTest.replacement !== window.runMarkerTest.context`), true,
        "changing contexts must create a new marker");
      await runAndCheck(page);
      assert.deepEqual(await page.eval(`(() => {
        const range = window.runMarkerTest.replacement.find();
        const position = ({line, ch}) => ({line, ch});
        return range && {from: position(range.from), to: position(range.to)};
      })()`),
        {from: {line: 0, ch: 0}, to: {line: 1, ch: 0}},
        "Run must preserve the replacement context marker");
    });

  test("a failed Run preserves editor state and the corrected type-check run clears error markers",
    { timeout: 180000 }, async () => {
      const page = getSession().page;
      await install(page, PROGRAM + "marker-test-object = {}\nmarker-test-object.x\n");
      await page.eval(`(() => {
        const cm = window.CPO.editor.cm;
        cm.setCursor(cm.lastLine(), 0);
        window.runMarkerTest = {
          context: cm.getAllMarks().find(mark => mark.className === "useline"),
          highlight: cm.markText(
            {line: 3, ch: 2}, {line: 3, ch: 7}, {className: "previous-run-highlight"})
        };
        cm.foldCode(2, null, "fold");
        window.runMarkerTest.before = new Set(cm.getAllMarks());
      })()`);
      assert.equal(await page.eval("!!window.runMarkerTest.context"), true,
        "the context line must have a marker before Run");
      const editorState = `[
        !!window.CPO.editor.cm.isFolded({line: 3, ch: 3}),
        !!window.runMarkerTest.context.find(),
        !!window.runMarkerTest.highlight.find()
      ]`;
      assert.deepEqual(await page.eval(editorState), [true, true, true],
        "the fold, context marker, and old highlight must exist before Run");

      await page.eval("window.PA.clearOutput()");
      await page.eval("window.PA.run()");
      await waitOrFail(page,
        `window.PA.compileErrorPresent() && ${outputHas("did not have a field")} && ${IDLE}`,
        60000, "the expected runtime error did not render");
      assert.deepEqual(await page.eval(editorState), [true, true, false],
        "a failed Run must still preserve editor state and clear the old highlight");

      // Correct the definition without touching the failing field access or folded
      // function. Actual source markers from error rendering must survive the edit
      // so this checks their cleanup by Run, rather than by replacing the document.
      await page.eval(`(() => {
        const cm = window.CPO.editor.cm;
        const diagnostics = cm.getAllMarks().filter(mark => !window.runMarkerTest.before.has(mark));
        const line = cm.lastLine() - 2;
        const ch = cm.getLine(line).indexOf("{}");
        cm.replaceRange("{x: 1}", {line, ch}, {line, ch: ch + 2});
        window.runMarkerTest.diagnostics = diagnostics.filter(mark => !!mark.find());
      })()`);
      assert.ok(await page.eval("window.runMarkerTest.diagnostics.length > 0"),
        "error rendering must leave source markers to clear on the next Run");
      assert.deepEqual(await page.eval(editorState), [true, true, false]);

      await runAndCheck(page, "window.PA.runTypeCheck()");
      assert.deepEqual(await page.eval(editorState), [true, true, false]);
      assert.equal(await page.eval("window.runMarkerTest.diagnostics.some(mark => !!mark.find())"), false,
        "the successful type-check run must clear markers from the previous error");
    });
};
