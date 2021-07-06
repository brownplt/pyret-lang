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
