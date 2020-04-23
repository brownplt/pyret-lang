import { UnControlled as CodeMirror } from 'react-codemirror2';

export type Chunk = {
  startLine: number,
  text: string,
  editor: CodeMirror.Editor | undefined,
  id: number,
};

export function getStartLineForIndex(chunks : Chunk[], index : number) {
  if (index === 0) { return 0; }

  return chunks[index - 1].startLine + chunks[index - 1].text.split('\n').length;
}

export function newId() {
  return Math.floor(new Date().valueOf() + 1000 * Math.random());
}
