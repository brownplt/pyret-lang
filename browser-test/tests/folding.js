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

const NESTED_PROGRAM = `use context starter2024

fun outer(n :: Number) -> Number:
  fun inner(m :: Number) -> Number:
    m + m
  end
  inner(n)
end

fun increment(n :: Number) -> Number:
  n + 1
end

check:
  outer(3) is 6
  increment(4) is 5
end
`;

async function runAndCheck(page) {
  await page.eval("window.PA.clearOutput()");
  await page.eval("window.PA.run()");
  await waitOrFail(page, `window.PA.doneRendering() && ${IDLE}`, 60000, "Run did not finish");
  assert.equal((await page.eval("window.PA.shipshapeResult()")).shipshape, true);
}

module.exports = function registerFolding(getSession) {
  test("Run keeps a function folded and clears its old highlight", { timeout: 120000 }, async () => {
    const page = getSession().page;
    await install(page, PROGRAM);
    await page.eval("window.PA.clearOutput()");

    // Put the highlight inside the fold so both marks cover the same character.
    await page.eval(`(() => {
      const cm = window.CPO.editor.cm;
      cm.setCursor(cm.lastLine(), 0);
      cm.markText({line: 3, ch: 2}, {line: 3, ch: 7}, {className: "previous-run-highlight"});
      cm.foldCode(2, null, "fold");
    })()`);
    const body = "{line: 3, ch: 3}";
    const folded = `!!window.CPO.editor.cm.isFolded(${body})`;
    const highlighted = `window.CPO.editor.cm.findMarksAt(${body})
      .some(mark => mark.className === "previous-run-highlight")`;
    assert.equal(await page.eval(folded), true, "the function must start folded");
    assert.equal(await page.eval(highlighted), true, "the highlight must exist before Run");

    await page.eval("document.getElementById('runButton').click()");
    await waitOrFail(page, `window.PA.doneRendering() && ${IDLE}`, 60000, "Run did not finish");
    assert.equal((await page.eval("window.PA.shipshapeResult()")).shipshape, true);
    assert.equal(await page.eval(folded), true, "Run unfolded the function");
    assert.equal(await page.eval(highlighted), false, "Run left the old highlight in place");
  });

  test("Run preserves registered ranges and bookmarks while clearing overlapping temporary marks",
    { timeout: 180000 }, async () => {
      const page = getSession().page;
      await install(page, PROGRAM);
      await page.eval(`(() => {
        const cm = window.CPO.editor.cm;
        const preserve = window.CPO.editorMarkers.preserveOnRun;
        window.runMarkerTest = {
          range: preserve(cm.markText({line: 3, ch: 2}, {line: 3, ch: 7}, {
            className: "persistent-annotation",
            attributes: {title: "keep this annotation"}
          })),
          bookmark: preserve(cm.setBookmark({line: 6, ch: 0}))
        };
      })()`);

      for (let run = 1; run <= 2; run++) {
        await page.eval(`(() => {
          const cm = window.CPO.editor.cm;
          window.runMarkerTest.temporary = [
            cm.markText({line: 3, ch: 2}, {line: 3, ch: 7}, {
              className: "previous-run-highlight",
              attributes: {title: "clear this annotation"}
            }),
            cm.setBookmark({line: 6, ch: 0})
          ];
        })()`);
        assert.deepEqual(await page.eval("window.runMarkerTest.temporary.map(mark => !!mark.find())"),
          [true, true], "temporary markers must exist before Run");

        await runAndCheck(page);
        assert.deepEqual(await page.eval(`(() => {
          const state = window.runMarkerTest;
          const position = ({line, ch}) => ({line, ch});
          const range = state.range.find();
          const bookmark = state.bookmark.find();
          return {
            range: range && {from: position(range.from), to: position(range.to)},
            title: state.range.attributes.title,
            bookmark: bookmark && position(bookmark),
            temporary: state.temporary.map(mark => !!mark.find())
          };
        })()`), {
          range: {from: {line: 3, ch: 2}, to: {line: 3, ch: 7}},
          title: "keep this annotation",
          bookmark: {line: 6, ch: 0},
          temporary: [false, false]
        }, `Run ${run} must preserve only the registered markers`);
      }

      // Preservation applies only to Run; owners must still be able to clear marks.
      await page.eval(`(() => {
        window.runMarkerTest.range.clear();
        window.runMarkerTest.bookmark.clear();
      })()`);
      assert.deepEqual(await page.eval(`[
        !!window.runMarkerTest.range.find(), !!window.runMarkerTest.bookmark.find()
      ]`), [false, false]);
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

  test("nested and separate folds survive repeated Run and can still unfold and refold",
    { timeout: 240000 }, async () => {
      const page = getSession().page;
      await install(page, NESTED_PROGRAM);
      await page.eval(`(() => {
        const cm = window.CPO.editor.cm;
        cm.setCursor(cm.lastLine(), 0);
        window.runMarkerTest = {
          fold(line) {
            const before = new Set(cm.getAllMarks());
            cm.foldCode(line, null, "fold");
            return cm.getAllMarks().find(mark => !before.has(mark));
          }
        };
        window.runMarkerTest.inner = window.runMarkerTest.fold(3);
        window.runMarkerTest.outer = window.runMarkerTest.fold(2);
        window.runMarkerTest.separate = window.runMarkerTest.fold(9);
      })()`);
      const foldsPresent = `[
        !!window.runMarkerTest.inner.find(),
        !!window.runMarkerTest.outer.find(),
        !!window.runMarkerTest.separate.find()
      ]`;
      assert.equal(await page.eval(`!!window.runMarkerTest.inner &&
        !!window.runMarkerTest.outer && !!window.runMarkerTest.separate &&
        window.runMarkerTest.inner !== window.runMarkerTest.outer`), true,
        "three distinct folds must exist before Run");

      for (let run = 1; run <= 2; run++) {
        await page.eval(`(() => {
          window.runMarkerTest.highlight = window.CPO.editor.cm.markText(
            {line: 4, ch: 4}, {line: 4, ch: 9}, {className: "previous-run-highlight"});
        })()`);
        assert.equal(await page.eval("!!window.runMarkerTest.highlight.find()"), true);
        await runAndCheck(page);
        assert.deepEqual(await page.eval(foldsPresent), [true, true, true],
          `Run ${run} must preserve nested and separate folds`);
        assert.equal(await page.eval("!!window.runMarkerTest.highlight.find()"), false,
          `Run ${run} must clear the highlight inside both nested folds`);
      }

      await page.eval("window.CPO.editor.cm.foldCode(2, null, 'unfold')");
      assert.deepEqual(await page.eval(foldsPresent), [true, false, true],
        "unfolding the outer function must retain its inner fold and the separate fold");
      await page.eval("window.CPO.editor.cm.foldCode(3, null, 'unfold')");
      assert.deepEqual(await page.eval(foldsPresent), [false, false, true]);

      await page.eval(`(() => {
        window.runMarkerTest.inner = window.runMarkerTest.fold(3);
        window.runMarkerTest.outer = window.runMarkerTest.fold(2);
      })()`);
      assert.deepEqual(await page.eval(foldsPresent), [true, true, true]);
      await runAndCheck(page);
      assert.deepEqual(await page.eval(foldsPresent), [true, true, true],
        "new fold markers created by refolding must also survive Run");
    });

  test("a failed Run preserves editor state and the corrected type-check run clears error markers",
    { timeout: 180000 }, async () => {
      const page = getSession().page;
      await install(page, PROGRAM + "marker-test-object = {}\nmarker-test-object.x\n");
      await page.eval(`(() => {
        const cm = window.CPO.editor.cm;
        cm.setCursor(cm.lastLine(), 0);
        window.runMarkerTest = {
          persistent: window.CPO.editorMarkers.preserveOnRun(cm.markText(
            {line: 3, ch: 2}, {line: 3, ch: 7}, {className: "persistent-annotation"})),
          highlight: cm.markText(
            {line: 3, ch: 2}, {line: 3, ch: 7}, {className: "previous-run-highlight"})
        };
        cm.foldCode(2, null, "fold");
        window.runMarkerTest.before = new Set(cm.getAllMarks());
      })()`);
      const editorState = `[
        !!window.CPO.editor.cm.isFolded({line: 3, ch: 3}),
        !!window.runMarkerTest.persistent.find(),
        !!window.runMarkerTest.highlight.find()
      ]`;
      assert.deepEqual(await page.eval(editorState), [true, true, true],
        "the fold, persistent annotation, and old highlight must exist before Run");

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

      await page.eval("window.PA.clearOutput()");
      await page.eval("window.PA.runTypeCheck()");
      await waitOrFail(page, `window.PA.doneRendering() && ${IDLE}`, 60000,
        "the corrected type-check run did not finish");
      assert.equal((await page.eval("window.PA.shipshapeResult()")).shipshape, true);
      assert.deepEqual(await page.eval(editorState), [true, true, false]);
      assert.equal(await page.eval("window.runMarkerTest.diagnostics.some(mark => !!mark.find())"), false,
        "the successful type-check run must clear markers from the previous error");
      await page.eval("window.runMarkerTest.persistent.clear()");
    });
};
