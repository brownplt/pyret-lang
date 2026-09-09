const cp = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');

const LANG = process.cwd();
const FIXTURES = path.join(LANG, 'tests/loader-tests/fixtures');
const PACKAGE = 'pyret-test-files';
const PACKAGE_SRC = path.join(LANG, 'tests/pyret/tests/node_modules', PACKAGE);
const OUT = path.join(LANG, 'tests/loader-tests/out');
const LIB_COMPILED = path.join(LANG, 'build/phaseA/lib-compiled');
const TIMEOUT = 300000;

const COMPILERS = {
  pyret: path.join(LANG, 'build/phaseA/pyret.jarr'),
  ts: path.join(LANG, 'build/ts-compiler/pyret.js'),
};
const backend = process.env.LOADER_BACKEND || 'pyret';

jest.setTimeout(TIMEOUT);

function compile(backend, program, outfile, compiledDir, extra = []) {
  const args = [
    COMPILERS[backend],
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
    ? path.join(root, '..', 'real', PACKAGE)
    : path.join(root, 'node_modules', PACKAGE);
  fs.cpSync(PACKAGE_SRC, target, { recursive: true });
  if (symlinkPackage) {
    fs.symlinkSync(target, path.join(root, 'node_modules', PACKAGE), 'dir');
  }
  return root;
}

function expectCompiled(c) {
  expect(c.stderr).toEqual('');
  expect(c.status).toEqual(0);
}

function expectGreeting(out) {
  const r = run(out);
  expect(r.status).toEqual(0);
  expect(r.stdout).toMatch(/root-of-package/);
}

const LIB_NAME = 'root-of-package.arr';
const LIB_KEY = artifactName('npm://' + PACKAGE + '/' + LIB_NAME, LIB_NAME);

let tmp;
beforeAll(() => {
  tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'pyret-loader-'));
  fs.mkdirSync(OUT, { recursive: true });
});
afterAll(() => {
  fs.rmSync(tmp, { recursive: true, force: true });
  fs.rmSync(OUT, { recursive: true, force: true });
});

{
  const t = (name) => {
    fs.mkdirSync(path.join(tmp, backend), { recursive: true });
    return path.join(tmp, backend, name);
  };
  const o = (name) => path.join(OUT, backend + '-' + name);

  describe(`[${backend}] file-reset-load-path`, () => {
    test('resolves relative to the working directory, not the importing file', () => {
      const out = o('reset.jarr');
      expectCompiled(compile(backend, 'tests/loader-tests/fixtures/reset/importer.arr', out, t('reset-cache')));
      const r = run(out);
      expect(r.status).toEqual(0);
      expect(r.stdout).toMatch(/reset-load-path-target-ok/);
    });

    test('the same path through file() does not resolve', () => {
      const c = compile(backend, 'tests/loader-tests/fixtures/reset/importer-file.arr', o('reset-file.jarr'), t('reset-file-cache'));
      expect(c.status).not.toEqual(0);
    });
  });

  describe(`[${backend}] npm package artifacts`, () => {
    test('are keyed by package-relative uri', () => {
      const proj = makeProject(t('a/proj'));
      const cache = t('cache-a');
      const out = o('a.jarr');
      expectCompiled(compile(backend, path.join(proj, 'main.arr'), out, cache));
      expect(fs.existsSync(path.join(cache, LIB_KEY + '-static.js'))).toBe(true);
      expect(fs.existsSync(path.join(cache, LIB_KEY + '-module.js'))).toBe(true);
      expectGreeting(out);
    });

    test('are reused from a read-only dir after the package moves and its sources get newer', () => {
      const cacheA = t('cache-a');
      expect(fs.existsSync(path.join(cacheA, LIB_KEY + '-static.js'))).toBe(true);

      const proj = makeProject(t('b/proj'));
      const future = new Date(Date.now() + 60 * 60 * 1000);
      fs.utimesSync(path.join(proj, 'node_modules', PACKAGE, LIB_NAME), future, future);

      const cacheB = t('cache-b');
      const out = o('b.jarr');
      expectCompiled(compile(backend, path.join(proj, 'main.arr'), out, cacheB, ['--compiled-read-only-dir', cacheA]));
      expect(artifactsFor(cacheB, LIB_NAME)).toEqual([]);
      expect(artifactsFor(cacheB, 'main.arr').length).toBeGreaterThan(0);
      expectGreeting(out);
    });

    test('resolve through a symlinked node_modules entry', () => {
      const proj = makeProject(t('c/proj'), { symlinkPackage: true });
      const cache = t('cache-c');
      const out = o('c.jarr');
      expectCompiled(compile(backend, path.join(proj, 'main.arr'), out, cache));
      expect(fs.existsSync(path.join(cache, LIB_KEY + '-static.js'))).toBe(true);
      expectGreeting(out);
    });
  });
}
