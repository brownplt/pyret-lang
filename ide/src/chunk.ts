import { UnControlled as CodeMirror } from 'react-codemirror2';

export type Chunk = {
  startLine: number,
  text: string,
  editor: CodeMirror.Editor | undefined,
};
