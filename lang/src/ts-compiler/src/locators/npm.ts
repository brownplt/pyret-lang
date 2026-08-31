/*
  Ported from: src/arr/compiler/locators/npm.arr

  The require-util trove resolves package names with browserify's `resolve`
  (basedir = current-load-path). Node's require.resolve with a `paths`
  override implements the same node-resolution algorithm, so we use it here
  instead of bundling browserify-resolve.
*/

import * as P from 'path';
import * as CS from '../compile-structs';
import * as F from './file';

export function makeNpmLocator(packageName: string, path: string, currentLoadPath: string): F.FileLocator {
  let packagePath: string;
  try {
    packagePath = require.resolve(packageName, { paths: [currentLoadPath] });
  } catch (err) {
    throw new Error(`Error resolving ${packageName} from ${currentLoadPath}: ${String(err)}`);
  }
  return F.fileLocator(
    P.resolve(P.join(P.dirname(packagePath), path)),
    CS.standardGlobals
  );
}
