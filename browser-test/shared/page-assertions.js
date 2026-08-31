/*
 * page-assertions.js
 *
 * A driver-agnostic, IN-PAGE port of the content predicates from
 * code.pyret.org/test-util/util.js. Everything here runs *inside the editor
 * document* (the CPO editor, whether it is a top-level page, an embed iframe,
 * or a vscode webview frame) and returns plain JSON. It has no dependency on
 * selenium or playwright, so the SAME predicate code can be evaluated by any
 * runner.
 *
 * Each function mirrors the corresponding logic in util.js. The mapping:
 *   util.setDefinitions / setCodemirror        -> PA.setDefinitions
 *   util.evalDefinitions                        -> PA.run / PA.runTypeCheck
 *   util.waitForBreakButton (elementIsDisabled) -> PA.breakDone
 *   util.pyretLoaded                            -> PA.pyretLoaded
 *   util.checkAllTestsPassed                    -> PA.shipshapeResult + waits
 *   util.testErrorRendersString                 -> PA.compileErrorPresent + PA.outputText
 *   util.testRunsAndHasCheckBlocks              -> PA.collectCheckBlocks
 *
 * This module exports the browser-side source as a string (SOURCE) that a
 * runner injects once to define `window.PA`, plus the node-side orchestration
 * in cpo-assertions.js calls into it.
 */

// The function below is stringified and injected into the editor frame.
function PYRET_PAGE_ASSERTIONS() {
  const PA = {
    // ---- readiness ----
    pyretLoaded() {
      const loader = document.getElementById("loader");
      if (!loader) return true; // loader already removed
      return getComputedStyle(loader).display === "none";
    },
    cmPresent() {
      const cm = document.querySelector(".CodeMirror");
      return !!(cm && cm.CodeMirror);
    },
    cmValue() {
      const cm = document.querySelector(".CodeMirror");
      return cm && cm.CodeMirror ? cm.CodeMirror.getValue() : null;
    },
    // Ready means Pyret has loaded AND whoever owns the initial contents is
    // done installing them. The editor declares the second fact itself
    // (EDITOR_CONTENTS_SETTLED: beforePyret's programLoaded for standalone
    // pages; the end of events.js reset() -- warm-start run included -- for
    // host-fed ones), so this predicate means the same thing in every env.
    // The previous shape inferred it from cmValue() !== "", which is honest
    // only where the reset is CM's sole writer (embed's controlled mode); the
    // vscode webview self-populates CM at boot, so the harness would start
    // driving while the host's reset was still in flight -- and a Run clicked
    // during its warm-start run is silently swallowed (the vscode x pyret CI
    // flake).
    editorReady() {
      return PA.pyretLoaded() && PA.cmPresent() && window.EDITOR_CONTENTS_SETTLED === true;
    },
    // stickError banners: every caller (runtime-bundle load failure, program
    // load failure, save failure) marks a genuinely broken editor. Crucially,
    // beforePyret's terminal load-failure path HIDES #loader while posting one
    // of these, so pyretLoaded()/editorReady() alone cannot tell a booted
    // runtime from a dead page -- the harness must check here too.
    stickyErrors() {
      return Array.from(document.querySelectorAll(".notificationArea .error"))
        .map((e) => (e.textContent || "").trim())
        .filter((t) => t !== "");
    },

    // ---- input (mirrors util.setCodemirror on the definitions CM) ----
    setDefinitions(code) {
      const CM = document.querySelector(".CodeMirror").CodeMirror;
      const first = CM.firstLine();
      const last = CM.lastLine();
      CM.replaceRange(code, { line: first, ch: 0 }, { line: last + 1, ch: 0 });
      return true;
    },
    run() {
      // Use the dropdown's "Run" item rather than #runButton. The run mode is
      // sticky (cpo-main.js: currentAction): after a type-check run, #runButton
      // re-runs type-checked. Upstream never sees this (a fresh browser per
      // suite), but we reuse one frame across suites, so we always select the
      // normal-run action explicitly. #select-run resets the mode AND runs.
      const sel = document.getElementById("select-run");
      if (sel) { sel.click(); } else { document.getElementById("runButton").click(); }
      return true;
    },
    // Empty #output before a run. CPO clears output on run, but when we reuse a
    // single frame across many sequential programs (instead of a fresh page per
    // test as upstream chart.js does), this removes any chance of reading the
    // previous run's stale .testing-summary / .check-results-done-rendering
    // before the new run repopulates them.
    clearOutput() {
      const out = document.getElementById("output");
      if (out) out.innerHTML = "";
      return true;
    },
    runTypeCheck() {
      document.getElementById("runDropdown").click();
      document.getElementById("select-tc-run").click();
      return true;
    },

    // ---- run lifecycle ----
    breakDone() {
      // util.waitForBreakButton waits until #breakButton is disabled.
      const b = document.getElementById("breakButton");
      return !!b && b.disabled === true;
    },
    testingSummaryPresent() {
      return !!document.querySelector("#output .testing-summary");
    },
    doneRendering() {
      return !!document.querySelector(".check-results-done-rendering");
    },
    compileErrorPresent() {
      return !!document.querySelector("#output .compile-error");
    },

    // ---- interactions REPL (mirrors util.evalPyret / evalPyretNoError). The
    // CPO editor hides interactions until the first run (beforePyret.js:1594
    // removes the `hideInteractions` body class on run), so these are used only
    // after a run has happened. ----
    replPromptVisible() {
      const pc = document.querySelector(".prompt-container");
      return !!(pc && pc.offsetParent !== null);
    },
    outputChildCount() {
      const out = document.getElementById("output");
      return out ? out.children.length : 0;
    },
    // Type code into the live REPL prompt and submit it (Enter), exactly like
    // util.evalPyret's setCodemirror + extraKeys.Enter on ".repl-prompt > .CodeMirror".
    evalAtRepl(code) {
      const cm = document.querySelector(".repl-prompt > .CodeMirror").CodeMirror;
      const first = cm.firstLine();
      const last = cm.lastLine();
      cm.replaceRange(code, { line: first, ch: 0 }, { line: last + 1, ch: 0 });
      cm.options.extraKeys.Enter(cm);
      return true;
    },
    // The last direct child of #output is the result of the most recent REPL
    // submission (util.evalPyret reads elements[elements.length-1]); report its
    // class and any .replOutput/.replTextOutput texts (util.evalPyretNoError).
    lastOutputChild() {
      const ch = document.getElementById("output").children;
      const el = ch[ch.length - 1];
      if (!el) return null;
      return {
        class: el.className,
        outputs: Array.from(el.querySelectorAll(".replOutput, .replTextOutput")).map((e) => e.innerText),
      };
    },

    // ---- tables (mirrors util.checkTableRendersCorrectly). The table program
    // prints a <pre> of JSON test specs {table,row,col,val}; for each we eval
    // the table expr and the value expr at the REPL and compare the rendered
    // cell's HTML to the value's rendered HTML. ----
    tablePre() {
      // util uses xpath 'pre' = a *direct child* <pre> of #output holding the
      // JSON spec (not the <pre> code snippets nested inside check results).
      const out = document.getElementById("output");
      if (!out) return null;
      const pre = Array.from(out.children).find((c) => c.tagName === "PRE");
      return pre ? pre.innerHTML : null;
    },
    // outerHTML of the first .replOutput/.replTextOutput in the last #output child
    lastReplOutputHTML() {
      const ch = document.getElementById("output").children;
      const el = ch[ch.length - 1];
      if (!el) return null;
      const r = el.querySelector(".replOutput, .replTextOutput");
      return r ? r.outerHTML : null;
    },
    // outerHTML of the row/col cell's span in the last REPL result's table
    // (mirrors //tbody/tr[row]/td[col]/span; row/col are 1-based)
    lastReplTableCellHTML(row, col) {
      const ch = document.getElementById("output").children;
      const el = ch[ch.length - 1];
      if (!el) return null;
      const r = el.querySelector(".replOutput, .replTextOutput");
      if (!r) return null;
      const tbody = r.querySelector("tbody");
      if (!tbody) return null;
      const tr = tbody.querySelectorAll(":scope > tr")[row - 1];
      if (!tr) return null;
      const td = tr.querySelectorAll(":scope > td")[col - 1];
      if (!td) return null;
      const span = td.querySelector("span");
      return span ? span.outerHTML : null;
    },

    // ---- output reading (mirrors the "remove output CodeMirrors then read
    // text" pattern used by testErrorRendersString / testRunsAndHasCheckBlocks
    // to avoid false positives from the code snippets) ----
    removeOutputCodeMirrors() {
      document.querySelectorAll("#output .CodeMirror").forEach((e) => e.remove());
      return true;
    },
    outputText() {
      const out = document.getElementById("output");
      return out ? out.innerText : "";
    },

    // ---- checkAllTestsPassed: "Looks shipshape" present => pass; otherwise
    // collect failed/errored blocks for the error message (mirrors util.js). ----
    shipshapeResult() {
      const out = document.getElementById("output");
      const text = out ? out.innerText : "";
      const shipshape = text.indexOf("Looks shipshape") !== -1;
      const failedEls = out
        ? Array.from(out.querySelectorAll(".check-block-failed, .check-block-errored"))
        : [];
      const failures = failedEls.map((cb) => {
        const header = cb.querySelector(".check-block-header");
        if (header) header.click();
        const tests = Array.from(cb.querySelectorAll(".check-block-test"));
        return tests.length === 0 ? [cb.innerText] : tests.map((t) => t.innerText);
      });
      return { shipshape, failures };
    },

    // ---- testRunsAndHasCheckBlocks: gather check blocks (skipping the first,
    // exactly like util.js's cbs.slice(1)), expand each, read its tests; if a
    // block has no .check-block-test children, treat it as specLen "Passed". ----
    collectCheckBlocks(specLens) {
      const out = document.getElementById("output");
      const cbs = out ? Array.from(out.querySelectorAll(".check-block")) : [];
      return cbs.slice(1).map((cb, i) => {
        const header = cb.querySelector(".check-block-header");
        if (header) header.click();
        const tests = Array.from(cb.querySelectorAll(".check-block-test"));
        if (tests.length === 0) {
          return new Array(specLens[i]).fill("Passed");
        }
        return tests.map((t) => t.innerText);
      });
    },
  };
  window.PA = PA;
  return true;
}

// Serialize the function body so a runner can inject it: `(<fn>)()`.
const SOURCE = "(" + PYRET_PAGE_ASSERTIONS.toString() + ")()";

module.exports = { SOURCE };
