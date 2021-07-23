import { NeverError } from './utils';

export type Srcloc =
  | { $name: 'builtin', 'module-name': string, 'asString': string, }
  | {
    $name: 'srcloc',
    'source': string,
    'start-line': number,
    'start-column': number,
    'start-char': number,
    'end-line': number,
    'end-column': number,
    'end-char': number
    'asString': string,
  };

export type Failure =
  | { $name: 'paragraph', 'contents': Array<Failure> }
  | { $name: 'bulleted-sequence', 'contents': Array<Failure> }
  | { $name: 'v-sequence', 'contents': Array<Failure> }
  | { $name: 'h-sequence', 'contents': Array<Failure>, 'sep': string }
  | {
    $name: 'h-sequence-sep',
    'contents': Array<Failure>, 'sep': string, 'last': string }
  | { $name: 'embed', 'val': any }
  | { $name: 'text', 'str': string }
  | { $name: 'loc', 'loc': Srcloc }
  | { $name: 'code', 'contents': Failure }
  | { $name: 'cmcode', 'loc': Srcloc }
  | {
    $name: 'loc-display',
    'loc': Srcloc, 'style': string, 'contents': Failure }
  | { $name: 'optional', 'contents': Failure }
  | {
    $name: 'highlight',
    'contents': Failure, 'locs': Array<Srcloc>, 'color': Number };

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
    default:
      throw new NeverError(failure);
  }
}
