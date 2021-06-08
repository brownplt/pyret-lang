/* eslint-disable */

import React from 'react';
import { Controlled as CodeMirror } from 'react-codemirror2';

require('pyret-codemirror-mode/mode/pyret');

export default function EditorPlayground() {
  const [value, setValue] = React.useState<string>('');
  const [chunks, setChunks] = React.useState<number[]>([0]);
  function onBeforeChange(editor: any, data: any, text: string) {
    setValue(text);
  }
  function clearNonsense(editor: any) {
    console.log("clearNonsense");
    const text = editor.getValue();
    const noPrompts = text.replace(/ ?⏎ Press Enter to run/g, '');
    console.log(noPrompts);
    setValue(noPrompts);
  }
  function onKeyDown(editor: any, event: any) {
    clearNonsense(editor);
    if (event.key === 'Enter') {
      const pos = (editor as any).getCursor();
      const token = editor.getTokenAt(pos);
      const line = editor.getLine(pos.line - 1);
      const newChunks: number[] = (
        chunks.map(line => (
          line >= pos.line ?  line + 1 : line
        ))
      );
      // Remove old prompts
      // As an alternative to finagling a useRef
      if (line.replace(/ /g, '') === '') {
        // Double enter
        if (!chunks.includes(pos.line - 1)) {
          console.log('RUNNING THE CHUNKS (theoretically)');
          // add chunk marker
          // In Uncontrolled (apparently), onKeyDown fires after onBeforeChange,
          // but on Controlled it fires before onBeforeChange. There are probably
          // cleaner ways to handle this, but what else would Enter do? Let's just
          // add 1 to the line number
          // Does changing chunks before simply re-setting it violate react state
          // invariants? There's no functional union on Set...
          newChunks.push(pos.line + 1);
          setChunks(chunks);
          editor.replaceSelection('---------------------\n');
        }
      // from DefChunk.tsx: handleEnter
      } else if (token.state.lineState.tokens.length === 0) {
        console.log('THE FINAL COUNTDOWN');
        editor.replaceSelection(' ⏎ Press Enter to run', 'start');
      } else {
        console.log('continue');
        // Easy way to drop text into this thing
        // My design instinct is to show nothing here: in the happy case, the
        // person is just writing a multiline expression and having a ball
        //editor.replaceSelection(' ⏎ Press Enter to show errors in unclosed block');
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
      onKeyUp={onKeyDown}
      onMouseDown={clearNonsense}
    />
  );
}
