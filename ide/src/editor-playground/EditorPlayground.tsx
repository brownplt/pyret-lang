import React from 'react';
import { Controlled as CodeMirror } from 'react-codemirror2';

require('pyret-codemirror-mode/mode/pyret');

export default function EditorPlayground() {
  const [value, setValue] = React.useState<string>('');
  const [chunks, setChunks] = React.useState<Set<number>>(new Set([0]));
  function onBeforeChange(editor: any, data: any, text: string) {
    setValue(text);
  }
  function onKeyDown(editor: any, event: any) {
    if (event.key === 'Enter') {
      // from DefChunk.tsx: handleEnter
      const pos = (editor as any).getCursor();
      const token = editor.getTokenAt(pos);
      if ((event as any).shiftKey) {
        // nothing?
      } else if (token.state.lineState.tokens.length === 0) {
        // add chunk marker
        // In Uncontrolled (apparently), onKeyDown fires after onBeforeChange,
        // but on Controlled it fires before onBeforeChange. There are probably
        // cleaner ways to handle this, but what else would Enter do? Let's just
        // add 1 to the line number
        // Does changing chunks before simply re-setting it violate react state
        // invariants? There's no functional union on Set...
        chunks.add(pos.line + 1);
        setChunks(chunks);
      }
    }
  }
  console.log(chunks);
  return (
    <CodeMirror
      value={value}
      options={{
        mode: 'pyret',
        lineNumbers: true,
      }}
      onBeforeChange={onBeforeChange}
      onKeyDown={onKeyDown}
    />
  );
}
