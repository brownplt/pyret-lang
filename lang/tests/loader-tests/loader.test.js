const cp = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');

const LANG = process.cwd();
const PYRET = path.join(LANG, 'build/phaseA/pyret.jarr');
const FIXTURES = path.join(LANG, 'tests/loader-tests/fixtures');
const LIB_COMPILED = path.join(LANG, 'build/phaseA/lib-compiled');
const OUT = path.join(LANG, 'tests/loader-tests/out');
const TIMEOUT = 300000;

jest.setTimeout(TIMEOUT);

function compile(program, outfile, compiledDir, extra = []) {
  const args = [
    PYRET,
    '--build-runnable', program,
    '--outfile', outfile,
    '--builtin-js-dir', 'src/js/trove',
    '--builtin-arr-dir', 'src/arr/trove',
    '--require-config', 'src/scripts/standalone-configA.json',
    '--compiled-dir', compiledDir,
    '--compiled-read-only-dir', LIB_COMPILED,
    '-no-check-mode',
  ].concat(extra);
  return cp.spawnSync('node', args, { cwd: LANG, encoding: 'utf8', timeout: TIMEOUT });
}

function run(outfile) {
  return cp.spawnSync('node', [outfile], { encoding: 'utf8', timeout: TIMEOUT });
}

function artifactName(uri, name) {
  return name + '-' + crypto.createHash('sha256').update(uri).digest('hex');
}

function artifactsFor(dir, name) {
  return fs.readdirSync(dir).filter((f) => f.startsWith(name + '-'));
}

function makeProject(root, { symlinkPackage } = {}) {
  fs.mkdirSync(path.join(root, 'node_modules'), { recursive: true });
  fs.copyFileSync(path.join(FIXTURES, 'npm-main.arr'), path.join(root, 'main.arr'));
  const target = symlinkPackage
    ? path.join(root, '..', 'real', 'fake-pkg')
    : path.join(root, 'node_modules', 'fake-pkg');
  fs.cpSync(path.join(FIXTURES, 'fake-pkg'), target, { recursive: true });
  if (symlinkPackage) {
    fs.symlinkSync(target, path.join(root, 'node_modules', 'fake-pkg'), 'dir');
  }
  return root;
}

let tmp;
beforeAll(() => {
  tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'pyret-loader-'));
  fs.mkdirSync(OUT, { recursive: true });
});
afterAll(() => {
  fs.rmSync(tmp, { recursive: true, force: true });
  fs.rmSync(OUT, { recursive: true, force: true });
});

describe('file-reset-load-path', () => {
  test('resolves relative to the working directory, not the importing file', () => {
    const out = path.join(OUT, 'reset.jarr');
    const c = compile('tests/loader-tests/fixtures/reset/importer.arr', out, path.join(tmp, 'reset-cache'));
    expect(c.stderr).toEqual('');
    expect(c.status).toEqual(0);
    const r = run(out);
    expect(r.status).toEqual(0);
    expect(r.stdout).toMatch(/reset-load-path-target-ok/);
  });

  test('the same path through file() does not resolve', () => {
    const out = path.join(OUT, 'reset-file.jarr');
    const c = compile('tests/loader-tests/fixtures/reset/importer-file.arr', out, path.join(tmp, 'reset-file-cache'));
    expect(c.status).not.toEqual(0);
  });
});

describe('npm package artifacts', () => {
  const uri = 'npm://fake-pkg/lib.arr';
  const key = artifactName(uri, 'lib.arr');

  test('are keyed by package-relative uri', () => {
    const proj = makeProject(path.join(tmp, 'a', 'proj'));
    const cache = path.join(tmp, 'cache-a');
    const out = path.join(OUT, 'a.jarr');
    const c = compile(path.join(proj, 'main.arr'), out, cache);
    expect(c.stderr).toEqual('');
    expect(c.status).toEqual(0);
    expect(fs.existsSync(path.join(cache, key + '-static.js'))).toBe(true);
    expect(fs.existsSync(path.join(cache, key + '-module.js'))).toBe(true);
    const r = run(out);
    expect(r.status).toEqual(0);
    expect(r.stdout).toMatch(/hello from fake-pkg/);
  });

  test('are reused from a read-only dir after the package moves and its sources get newer', () => {
    const cacheA = path.join(tmp, 'cache-a');
    expect(fs.existsSync(path.join(cacheA, key + '-static.js'))).toBe(true);

    const proj = makeProject(path.join(tmp, 'b', 'proj'));
    const future = new Date(Date.now() + 60 * 60 * 1000);
    fs.utimesSync(path.join(proj, 'node_modules', 'fake-pkg', 'lib.arr'), future, future);

    const cacheB = path.join(tmp, 'cache-b');
    const out = path.join(OUT, 'b.jarr');
    const c = compile(path.join(proj, 'main.arr'), out, cacheB, ['--compiled-read-only-dir', cacheA]);
    expect(c.stderr).toEqual('');
    expect(c.status).toEqual(0);
    expect(artifactsFor(cacheB, 'lib.arr')).toEqual([]);
    expect(artifactsFor(cacheB, 'main.arr').length).toBeGreaterThan(0);
    const r = run(out);
    expect(r.status).toEqual(0);
    expect(r.stdout).toMatch(/hello from fake-pkg/);
  });

  test('resolve through a symlinked node_modules entry', () => {
    const proj = makeProject(path.join(tmp, 'c', 'proj'), { symlinkPackage: true });
    const cache = path.join(tmp, 'cache-c');
    const out = path.join(OUT, 'c.jarr');
    const c = compile(path.join(proj, 'main.arr'), out, cache);
    expect(c.stderr).toEqual('');
    expect(c.status).toEqual(0);
    expect(fs.existsSync(path.join(cache, key + '-static.js'))).toBe(true);
    const r = run(out);
    expect(r.status).toEqual(0);
    expect(r.stdout).toMatch(/hello from fake-pkg/);
  });
});
