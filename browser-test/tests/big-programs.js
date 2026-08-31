/*
 * big-programs.js -- generated large-program specs that pin the compiler's
 * stack behavior in a real browser (fixed ~1MB stack, no --stack-size escape
 * hatch, unlike the CLI which respawns node with --stack-size=8192).
 *
 * Each shape below is one the compiler must translate iteratively rather than
 * with one recursive activation per statement/arm/variant; each has overflowed
 * a fixed-size stack in the ANF pass at exactly these scales at some point
 * (the ~800-function program is the shape of the original bad-stack.arr
 * report). The programs are generated, not fixtures, so the scales are easy
 * to read and adjust; the check blocks make each spec assert real end-to-end
 * behavior ("Looks shipshape") rather than just "it compiled".
 *
 * Unlike the other suites, these specs are NOT extracted from
 * code.pyret.org/test/*.js -- they are stress shapes specific to fixed-stack
 * environments, so they live here in the harness (which keeps the "nothing
 * under code.pyret.org/ is modified" property). The spec objects use the same
 * {kind, name, code, options} shape load-cpo-specs records, so dispatch.js
 * runs them through the same assertions.
 *
 * Timeouts: these compile in a few seconds on the ts backend but the stock
 * Pyret-hosted compiler is slower on programs this size, and CI runners are
 * slow; the generous per-spec budget only matters on failure.
 */

function manyFuns(n) {
  const lines = [];
  for (let i = 0; i < n; i++) {
    lines.push(`fun pad-${i}(x :: Number) -> Number: x + ${i} end`);
  }
  lines.push("check:");
  lines.push("  pad-0(1) is 1");
  lines.push(`  pad-${n - 1}(1) is ${n}`);
  lines.push("end");
  return lines.join("\n");
}

function manyAskArms(n) {
  const lines = [];
  lines.push("fun classify(x :: Number) -> Number:");
  lines.push("  ask:");
  for (let i = 0; i < n; i++) {
    lines.push(`    | x == ${i} then: ${i}`);
  }
  lines.push("    | otherwise: -1");
  lines.push("  end");
  lines.push("end");
  lines.push("check:");
  lines.push("  classify(0) is 0");
  lines.push(`  classify(${n - 1}) is ${n - 1}`);
  lines.push(`  classify(${n}) is -1`);
  lines.push("end");
  return lines.join("\n");
}

function manyDataVariants(n) {
  const lines = [];
  lines.push("data Big:");
  for (let i = 0; i < n; i++) {
    lines.push(`  | v${i}(a${i} :: Number)`);
  }
  lines.push("end");
  lines.push("check:");
  lines.push("  v0(1).a0 is 1");
  lines.push(`  v${n - 1}(2).a${n - 1} is 2`);
  lines.push(`  is-v${n - 1}(v0(1)) is false`);
  lines.push("end");
  return lines.join("\n");
}

const TIMEOUT = 240000;

function specs() {
  return [
    { kind: "allTestsPass", name: "many-top-level-funs-1000", code: manyFuns(1000), options: { timeout: TIMEOUT } },
    { kind: "allTestsPass", name: "many-ask-arms-800", code: manyAskArms(800), options: { timeout: TIMEOUT } },
    { kind: "allTestsPass", name: "many-data-variants-400", code: manyDataVariants(400), options: { timeout: TIMEOUT } },
  ];
}

module.exports = { specs, manyFuns, manyAskArms, manyDataVariants };
