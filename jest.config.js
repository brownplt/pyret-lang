module.exports = {
  // NOTE(joe): jest only points at tests-new
  testPathIgnorePatterns: [
    'tools/',
    'tests/',
    'src/*',
    'ide/src/*'
  ],
  testEnvironment: 'node',
  // NOTE(alex): Converts TS files to CommonJS syntax (ignores JS files)
  //   See: https://kulshekhar.github.io/ts-jest/docs/presets
  preset: 'ts-jest'
}
