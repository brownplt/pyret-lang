'use strict';

// Runs the frozen cases in a worker thread so that a js-numbers call that
// never returns (drydock's remainder(1/2, 0) loops forever) is reported as
// a timed-out failure instead of hanging the suite.

const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
const S = require('./suite');
const { loadLibrary } = require('./load');

const DEFAULT_TIMEOUT_MS = Number(process.env.JSNUMS_TIMEOUT || 10000);

if (!isMainThread && workerData.single) {
  const { libPath, bits, op, args } = workerData;
  const { lib } = loadLibrary(libPath, bits);
  const a = S.evaluate(lib, op, args);
  parentPort.postMessage({ kind: a.kind, cls: a.cls, message: a.message, text: S.describe(a) });
} else if (!isMainThread) {
  const { libPath, bits, files, filterText } = workerData;
  const { lib } = loadLibrary(libPath, bits);
  for (const { name, start } of files) {
    const data = S.readCaseFile(name);
    for (let i = start; i < data.cases.length; i++) {
      const c = data.cases[i];
      if (filterText && !c.id.includes(filterText)) continue;
      parentPort.postMessage({ type: 'start', name, index: i });
      const actual = S.evaluate(lib, data.op, c.args);
      const r = S.compare(data.op, c.args, c, actual);
      // one message per case so nothing is lost when a later case hangs
      parentPort.postMessage({ type: 'result', name, result: { id: c.id, family: c.family, args: c.args, expect: c.expect, mode: c.mode, ok: r.ok, why: r.why, ulp: r.ulp, negZero: r.negZero, representation: r.representation, actual: { kind: actual.kind, text: S.describe(actual) } } });
    }
  }
  parentPort.postMessage({ type: 'done' });
}

// Resolves to Map(fileName -> results[]) for one library and digit config.
function runConfig(libPath, bits, fileNames, opts) {
  const timeout = (opts && opts.timeout) || DEFAULT_TIMEOUT_MS;
  const filterText = opts && opts.filterText;
  const out = new Map();
  for (const n of fileNames) out.set(n, []);
  return new Promise((resolve, reject) => {
    let files = fileNames.map(name => ({ name, start: 0 }));
    let current = null, timer = null, worker = null;
    const spawn = () => {
      worker = new Worker(__filename, { workerData: { libPath, bits, files, filterText } });
      worker.on('message', m => {
        if (m.type === 'start') {
          current = m;
          clearTimeout(timer);
          timer = setTimeout(onTimeout, timeout);
        } else if (m.type === 'result') {
          out.get(m.name).push(m.result);
        } else if (m.type === 'done') {
          clearTimeout(timer);
          resolve(out);
        }
      });
      worker.on('error', e => { clearTimeout(timer); reject(e); });
    };
    const onTimeout = () => {
      worker.terminate();
      const data = S.readCaseFile(current.name);
      const c = data.cases[current.index];
      out.get(current.name).push({ id: c.id, family: c.family, args: c.args, expect: c.expect, mode: c.mode, ok: false, why: 'timed out after ' + timeout + ' ms (js-numbers did not return)', actual: { kind: 'timeout', text: 'timeout' } });
      const idx = files.findIndex(f => f.name === current.name);
      files = [{ name: current.name, start: current.index + 1 }].concat(files.slice(idx + 1));
      spawn();
    };
    spawn();
  });
}

// Map(id@bits -> result) over every case file and the given configs.
async function runAll(libPath, configs, opts) {
  const names = S.caseFiles();
  const all = new Map();
  for (const bits of configs) {
    const per = await runConfig(libPath, bits, names, opts);
    for (const [name, results] of per) {
      const op = name.replace(/\.json$/, '');
      for (const r of results) all.set(r.id + '@' + bits, Object.assign({ op, bits, file: name }, r));
    }
  }
  return all;
}

// Evaluate one op in a worker; resolves to the classification or {kind: 'timeout'}.
function evaluateInWorker(libPath, bits, op, args, timeout) {
  return new Promise((resolve, reject) => {
    const worker = new Worker(__filename, { workerData: { single: true, libPath, bits, op, args } });
    const timer = setTimeout(() => { worker.terminate(); resolve({ kind: 'timeout', text: 'timed out after ' + (timeout || DEFAULT_TIMEOUT_MS) + ' ms' }); }, timeout || DEFAULT_TIMEOUT_MS);
    worker.on('message', m => { clearTimeout(timer); resolve(m); });
    worker.on('error', e => { clearTimeout(timer); reject(e); });
  });
}

module.exports = { runConfig, runAll, evaluateInWorker, DEFAULT_TIMEOUT_MS };
