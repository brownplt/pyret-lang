import { UnControlled as CodeMirror } from 'react-codemirror2';

export type Chunk = {
  startLine: number,
  id: string,
  text: string,
  editor: CodeMirror.Editor | undefined,
};
