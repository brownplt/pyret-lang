import { Controlled as CodeMirror } from 'react-codemirror2';

export type Lint =
  ({ status: 'failed', failures: string[], highlights: number[][] }
  | { status: 'succeeded' }
  | { status: 'notLinted' });

export type Chunk = {
  startLine: number,
  text: string,
  id: number,
  lint: Lint,
  editor: false | CodeMirror.Editor;
};

export function getStartLineForIndex(chunks : Chunk[], index : number) {
  if (index === 0) { return 0; }

  return chunks[index - 1].startLine + chunks[index - 1].text.split('\n').length;
}

export function newId() {
  return Math.floor(new Date().valueOf() + 1000 * Math.random());
}
