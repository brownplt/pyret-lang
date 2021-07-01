/* The container that chunks live in. Handles setting up and reordering chunks.
   Most of the interesting UI considerations about the chunk editor happens in
   DefChunks.tsx, not here. */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import CodeMirror from 'codemirror';
import { UnControlled } from 'react-codemirror2';
import { Action } from './action';
import {
  State,
  EditorResponseLoop,
} from './state';
import { Chunk } from './chunk';
import { RHSObjects } from './rhsObject';
import DefChunk from './DefChunk';

type StateProps = {
  chunks: Chunk[],
  focusedChunk: number | undefined,
  rhs: RHSObjects,
  debugBorders: boolean,
  editorResponseLoop: EditorResponseLoop,
};

type DispatchProps = {
  setRHS: () => void,
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
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunksProps = PropsFromRedux & DispatchProps & StateProps;

function DefChunks({
  chunks,
  focusedChunk,
  debugBorders,
}: DefChunksProps) {
  const doc = React.useState<CodeMirror.Doc>(() => (
    CodeMirror.Doc('original\n\n\n\n\n\n\n\n\n', 'pyret')
  ))[0];
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
    width: '35em',
    margin: '2em auto',
  };
  return (
    <>
      <div style={togetherStyle}>
        {allChunks}
      </div>
      <UnControlled
        className="new-expr"
        options={{
          mode: 'pyret',
          theme: 'default',
          lineWrapping: true,
          autofocus: true,
        }}
        onKeyDown={(_editor: CodeMirror.Editor, event: Event) => {
          switch ((event as any).key) {
            case 'Enter':
              console.log('the money happens');
              break;
            case 'ArrowUp':
              console.log('more money');
              break;
            default:
          }
        }}
        autoCursor
      />
    </>
  );
}

export default connector(DefChunks);
