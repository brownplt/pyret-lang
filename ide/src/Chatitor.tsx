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
import DefChunk from './DefChunk';
import { BackendCmd, Effect } from './effect';

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

function DefChunks({
  chunks,
  focusedChunk,
  debugBorders,
  setChunks,
  enqueueEffect,
}: DefChunksProps) {
  const doc = React.useState<CodeMirror.Doc>(() => {
    const wholeProgram = chunks.reduce((acc, { editor }) => (
      `${acc + editor.getValue()}\n`
    ), '');
    return CodeMirror.Doc(wholeProgram, 'pyret');
  })[0];
  function setupChunk(chunk: Chunk, index: number) {
    const focused = focusedChunk === index;

    /* Returns the color of the drag handle */
    // eslint-disable-next-line
    function getBorderColor() {
      if (focused && chunk.errorState.status === 'failed') {
        return 'red';
      }

      if (!focused && chunk.errorState.status === 'failed') {
        return '#ff9999';
      }

      if (debugBorders === true) {
        if (focused && chunk.errorState.status === 'notLinted') {
          return 'orange';
        }

        if (!focused && chunk.errorState.status === 'notLinted') {
          return 'yellow';
        }
      }

      if (focused) {
        return 'lightgray';
      }

      return '#eee';
    }

    return (
      <DefChunk
        key={chunk.id}
        index={index}
        focused={focused}
        parent={doc}
      />
    );
  }

  const allChunks = chunks.map(setupChunk);

  const togetherStyle = {
    width: '30em',
    margin: '2em auto',
  };
  return (
    <div className="chatitor-container">
      <div style={{ gridRow: '1', width: '100%', overflowY: 'scroll' }}>
        <div style={togetherStyle}>
          {allChunks}
        </div>
      </div>
      <UnControlled
        className="new-expr"
        options={{
          mode: 'pyret',
          theme: 'default',
          lineWrapping: true,
          autofocus: true,
        }}
        editorDidMount={(editor) => {
          editor.setSize(null, 'auto');
        }}
        onKeyDown={((editor: CodeMirror.Editor & CodeMirror.Doc, event: Event) => {
          switch ((event as any).key) {
            case 'Enter': {
              const pos = editor.getCursor();
              const token = editor.getTokenAt(pos);
              if (token.state.lineState.tokens.length === 0) {
                const value = editor.getValue();
                console.log(value);
                const nextChunks: Chunk[] = [
                  ...chunks,
                  emptyChunk({
                    startLine: getStartLineForIndex(chunks, chunks.length),
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
            case 'ArrowUp':
              console.log('more money');
              break;
            default:
          }
        }) as any}
        autoCursor
      />
    </div>
  );
}

export default connector(DefChunks);
