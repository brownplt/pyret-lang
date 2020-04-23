import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import {
  DragDropContext, Droppable, Draggable, DropResult,
} from 'react-beautiful-dnd';
import { Action } from './action';
import {
  LintFailures,
  State,
} from './state';
import { Chunk, getStartLineForIndex } from './chunk';
import DefChunk from './DefChunk';

type stateProps = {
  lintFailures: LintFailures,
  highlights: number[][],
  name: string,
  chunks: Chunk[],
  chunkIndexCounter: number,
  focusedChunk: number | undefined,
};

type dispatchProps = {
  handleChunkEdit: any,
  handleReorder: any,
};

function mapStateToProps(state: State): stateProps {
  const {
    lintFailures,
    definitionsHighlights,
    currentFile,
    chunks,
    TMPchunkIndexCounter,
    focusedChunk,
  } = state;

  if (currentFile === undefined) {
    throw new Error('currentFile is undefined');
  }

  return {
    lintFailures,
    highlights: definitionsHighlights,
    name: currentFile,
    chunks,
    chunkIndexCounter: TMPchunkIndexCounter,
    focusedChunk,
  };
}

function mapDispatchToProps(dispatch: (action: Action) => any): dispatchProps {
  return {
    handleChunkEdit(
      chunks: Chunk[],
      chunkIndexCounter: number,
      index: number,
      text: string,
      shouldCreateNewChunk: boolean,
    ) {
      /* if (index === chunks.length) {
       *   const id = String(chunkIndexCounter);
       *   dispatch({ type: 'setChunkIndexCounter', chunkIndexCounter: chunkIndexCounter + 1 });
       *   newChunks = chunks.concat([{
       *     text,
       *     id,
       *     startLine: getStartLineForIndex(chunks, chunks.length),
       *     editor: undefined,
       *   }]);
       * } else */
      // {
      const newChunks: Chunk[] = chunks.map((p, ix) => {
        if (ix === index) {
          return {
            text,
            startLine: p.startLine,
            editor: p.editor,
          };
        }
        return p;
      });
      if (shouldCreateNewChunk) {
        console.log('SHOULD CREATE NEW CHUNK');
      } else {
        console.log('SHOULD NOT CREATE NEW CHUNK');
      }

      // }
      dispatch({ type: 'setChunks', chunks: newChunks });
      dispatch({ type: 'updateChunkContents', index, contents: text });
    },
    handleReorder(
      result: DropResult,
      chunks: Chunk[],
    ) {
      // Great examples! https://codesandbox.io/s/k260nyxq9v
      const reorder = (innerChunks: Chunk[], start: number, end: number) => {
        const newResult = Array.from(innerChunks);
        const [removed] = newResult.splice(start, 1);
        newResult.splice(end, 0, removed);
        return newResult;
      };
      if (result.destination === undefined) { return; }

      const newChunks = reorder(chunks, result.source.index, result.destination.index);

      for (let i = 0; i < newChunks.length; i += 1) {
        newChunks[i].startLine = getStartLineForIndex(newChunks, i);
      }

      console.log('newChunks', newChunks);

      dispatch({ type: 'setChunks', chunks: newChunks });
      const firstAffectedChunk = Math.min(result.source.index, result.destination.index);
      dispatch({
        type: 'updateChunkContents',
        index: firstAffectedChunk,
        contents: newChunks[firstAffectedChunk].text,
      });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunksProps = PropsFromRedux & dispatchProps & stateProps;

function DefChunks({
  handleChunkEdit,
  handleReorder,
  chunks,
  chunkIndexCounter,
  name,
  lintFailures,
  highlights,
  focusedChunk,
}: DefChunksProps) {
  const onChunkEdit = (index: number, text: string, shouldCreateNewChunk: boolean) => {
    handleChunkEdit(chunks, chunkIndexCounter, index, text, shouldCreateNewChunk);
  };
  const onDragEnd = (result: DropResult) => {
    if (result.destination !== null
        && result.source!.index !== result.destination!.index) {
      handleReorder(result, chunks);
    }
  };

  function setupChunk(chunk: Chunk, index: number) {
    const linesInChunk = chunk.text.split('\n').length;
    let chunkHighlights : number[][];
    const chunkName = `${name}_chunk_${index}`;
    let failures : string[] = [];
    if (chunkName in lintFailures) {
      failures = lintFailures[chunkName].errors;
    }
    if (highlights.length > 0) {
      chunkHighlights = highlights.filter(
        (h) => h[0] > chunk.startLine && h[0] <= chunk.startLine + linesInChunk,
      );
    } else {
      chunkHighlights = [];
    }
    return (
      <Draggable key={index} draggableId={String(index)} index={index}>
        {(draggableProvided) => (
          <div
            ref={draggableProvided.innerRef}
            // eslint-disable-next-line react/jsx-props-no-spreading
            {...draggableProvided.draggableProps}
          >
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                width: '100%',
              }}
            >
              <div
              // eslint-disable-next-line react/jsx-props-no-spreading
                {...draggableProvided.dragHandleProps}
                style={{
                  minWidth: '1.5em',
                  height: 'auto',
                  display: 'flex',
                  justifyContent: 'center',
                  alignItems: 'center',
                  borderLeft: '1px solid lightgray',
                  background: 'lightgray',
                  borderRadius: '75% 0% 0% 75%',
                  marginLeft: '0.5em',
                }}
              >
                ::
              </div>
              <DefChunk
                name={chunkName}
                failures={failures}
                highlights={chunkHighlights}
                key={index}
                index={index}
                onEdit={onChunkEdit}
                focused={focusedChunk === index}
              />
            </div>
          </div>
        )}
      </Draggable>
    );
  }

  const allChunks = chunks.map(setupChunk);

  return (
    <DragDropContext onDragEnd={onDragEnd}>
      <Droppable droppableId="droppable">
        {(provided) => (
          <div
            // eslint-disable-next-line react/jsx-props-no-spreading
            {...provided.droppableProps}
            ref={provided.innerRef}
          >
            {allChunks}
            {provided.placeholder}
          </div>
        )}
      </Droppable>
    </DragDropContext>
  );
}

export default connector(DefChunks);
