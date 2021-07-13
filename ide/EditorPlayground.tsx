/* eslint-disable */

import React from 'react';
import CM from 'codemirror';
import Chunk from './Chunk';

require('pyret-codemirror-mode/mode/pyret');

export default function EditorPlayground() {
  const [wholeDoc, _setWholeDoc] = React.useState<CM.Doc>(() => {
    const doc = CM.Doc('original\n\n\n\n\n\n\n\n\n', 'pyret');
    // Change seems to work on the whole document, but not beforeChange. i'm not
    // sure if this is because of how linkedDoc works or what, but it's not
    // documented
    CM.on(doc, 'change', onChange);
    return doc;
  });
  const [chunks, setChunks] = React.useState<JSX.Element[]>([(
    <Chunk
      parentEditor={wholeDoc}
      start={0}
      end={1}
      key="one"
    />
  )]);
  // This ref patterns sure feels a lot like i'm implementing setState but whatever....
  const chunksRef = React.useRef(chunks);
  function onChange(doc: CM.Doc, change: CM.EditorChangeCancellable) {
    // This represents a simple Enter:
    function isOneEmptyString(a: string[] | undefined) {
      if (a === undefined) {
        return false;
      }
      if (a.length === 1) {
        return a[0] === "";
      }
      return false;
    }
    const wasEnter = isOneEmptyString(change.removed) && change.text.length === 2;
    if (wasEnter) {
      const line = doc.getLine(change.from.line);
      if (line.replace(/ /g, '') === '') {
        // Double enter
        // TODO(luna): Detect chunk already exists(?) and other edge cases(?)
        if (true/*!chunks.includes(pos.line)*/) {
          console.log('Second enter! RUNNING THE CHUNKS (theoretically)');
          // add chunk marker
          // In Uncontrolled (apparently), onKeyDown fires after onBeforeChange,
          // but on Controlled it fires before onBeforeChange. There are probably
          // cleaner ways to handle this, but what else would Enter do? Let's just
          // add 1 to the line number
          // Does changing chunks before simply re-setting it violate react state
          // invariants? There's no functional union on Set...
          const newChunks = chunksRef.current.map(x => x);
          let thisIndex;
          let i = 0;
          // Does this preserve an order of some kind??
          doc.iterLinkedDocs((doc: CM.Doc) => {
            if (doc.firstLine() <= change.to.line && doc.lastLine() >= change.to.line) {
              thisIndex = i;
            }
            ++i;
          })
          if (thisIndex === undefined) {
            throw new Error('No document with this line');
          }
          // doc.replaceRange('', {line: doc.lastLine(), ch: 0}, {line: doc.lastLine(), ch: 0});
          newChunks.splice(thisIndex + 1, 0, <Chunk key={newChunks.length} parentEditor={wholeDoc} start={change.to.line + 2} end={change.to.line + 3} />);
          chunksRef.current = newChunks;
          setChunks(newChunks);
        }
      }
    }
  }
  console.log(chunks);
  return (
    <>
      {chunks}
    </>
  );
}
