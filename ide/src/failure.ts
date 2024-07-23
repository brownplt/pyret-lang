import { NeverError, Srcloc } from './utils';
import type { ErrorDisplay } from '../../src/runtime-arr/error-display.arr';

export type Failure = ErrorDisplay;

export function getLocs(failure: Failure): Srcloc[] {
  switch (failure.$name) {
    case 'paragraph':
    case 'bulleted-sequence':
    case 'v-sequence':
    case 'h-sequence':
    case 'h-sequence-sep':
      return failure.contents.flatMap(getLocs);
    case 'embed':
    case 'text':
      return [];
    case 'loc':
    case 'cmcode':
      return [failure.loc];
    case 'loc-display':
      return [...getLocs(failure.contents), failure.loc];
    case 'code':
    case 'optional':
      return getLocs(failure.contents);
    case 'highlight':
      return [...getLocs(failure.contents), ...failure.locs];
    case 'maybe-stack-loc':
      return [...getLocs(failure['contents-without-loc'])];
    default:
      throw new NeverError(failure);
  }
}
