/* The container that chunks live in. Handles setting up and reordering chunks.
   Most of the interesting UI considerations about the chunk editor happens in
   DefChunks.tsx, not here. */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import CodeMirror from 'codemirror';
import { UnControlled } from 'react-codemirror2';
import { Action, ChunksUpdate } from './action';
import {
  State,
  EditorResponseLoop,
} from './state';
import {
  Chunk, emptyChunk, getStartLineForIndex, lintSuccessState,
} from './chunk';
import { RHSObjects } from './rhsObject';
import { BackendCmd, Effect } from './effect';
import Chat from './Chat';

type StateProps = {
  chunks: Chunk[],
  focusedChunk: number | undefined,
  rhs: RHSObjects,
  debugBorders: boolean,
  editorResponseLoop: EditorResponseLoop,
};

type DispatchProps = {
  setRHS: () => void,
  setChunks: (chunks: ChunksUpdate) => void,
  enqueueEffect: (effect: Effect) => void,
};

function mapStateToProps(state: State): StateProps {
  const {
    chunks,
    focusedChunk,
    rhs,
    debugBorders,
    editorResponseLoop,
  } = state;

  return {
    chunks,
    focusedChunk,
    rhs,
    debugBorders,
    editorResponseLoop,
  };
}

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    setRHS() {
      dispatch({ type: 'update', key: 'rhs', value: 'make-outdated' });
    },
    setChunks(chunks: ChunksUpdate) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    enqueueEffect(effect: Effect) {
      dispatch({ type: 'enqueueEffect', effect });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunksProps = PropsFromRedux & DispatchProps & StateProps;

function Chatitor({
  chunks,
  setChunks,
  enqueueEffect,
}: DefChunksProps) {
  const doc = React.useState<CodeMirror.Doc>(() => {
    const wholeProgram = chunks.reduce((acc, { editor }) => (
      `${acc + editor.getValue()}\n`
    ), '');
    return CodeMirror.Doc(wholeProgram, 'pyret');
  })[0];
  const [mountedEditor, setEditor] = (
    React.useState<(CodeMirror.Editor & CodeMirror.Doc) | null>(null)
  );
  // UnControlled continues to have stale closures for no reason, ref is an easy
  // solution
  const chunksRef = React.useRef(chunks);
  chunksRef.current = chunks;
  function setupChunk(chunk: Chunk, index: number) {
    return (
      <Chat
        key={chunk.id}
        index={index}
        parent={doc}
        focusNewChat={() => mountedEditor?.getInputField().focus()}
      />
    );
  }

  const allChunks = chunks.map(setupChunk);

  const togetherStyle = {
    width: '40em',
    maxWidth: '70%',
    margin: '2em auto',
  };
  return (
    <div className="chatitor-container">
      <div style={{ gridRow: '1', width: '100%', overflowY: 'scroll' }}>
        <div style={togetherStyle}>
          {allChunks}
          <div style={{ clear: 'both' }} />
        </div>
      </div>
      <UnControlled
        className="new-expr"
        options={{
          mode: 'pyret',
          theme: 'default',
          lineWrapping: true,
          autofocus: true,
          matchBrackets: true,
          matchKeywords: true,
        } as any}
        editorDidMount={((editor: CodeMirror.Editor & CodeMirror.Doc) => {
          editor.setSize(null, 'auto');
          setEditor(editor);
        }) as (editor: CodeMirror.Editor) => void}
        onKeyDown={((editor: CodeMirror.Editor & CodeMirror.Doc, event: Event) => {
          switch ((event as any).key) {
            case 'Enter': {
              const pos = editor.getCursor();
              const token = editor.getTokenAt(pos);
              if (token.state.lineState.tokens.length === 0) {
                const value = editor.getValue();
                const nextChunks: Chunk[] = [
                  ...chunksRef.current,
                  emptyChunk({
                    startLine: getStartLineForIndex(chunksRef.current, chunksRef.current.length),
                    errorState: lintSuccessState,
                    editor: { getValue: () => value },
                  }),
                ];
                setChunks({ chunks: nextChunks, modifiesText: true });
                editor.setValue('');
                enqueueEffect({ effectKey: 'initCmd', cmd: BackendCmd.Run });
                event.preventDefault();
              }
              break;
            }
            case 'ArrowUp': {
              const pos = editor.getCursor();
              if (pos.line === 0) {
                const lastEditor = chunksRef.current[chunksRef.current.length - 1].editor;
                if ('getInputField' in lastEditor) {
                  lastEditor.getInputField().focus();
                }
              }
              break;
            }
            case 'Escape':
              editor.getInputField().blur();
              break;
            default:
          }
        }) as any}
        autoCursor
      />
    </div>
  );
}

export default connector(Chatitor);
