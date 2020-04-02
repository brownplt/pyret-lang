import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import {
  DragDropContext, Droppable, Draggable, DropResult,
} from 'react-beautiful-dnd';
import { Action } from './action';
import {
  LintFailures,
  Chunk,
  State,
} from './state';
import DefChunk from './DefChunk';

type stateProps = {
  lintFailures: LintFailures,
  highlights: number[][],
  name: string,
  chunks: Chunk[],
  chunkIndexCounter: number
};

type dispatchProps = {
  doMagicA: any,
  doMagicB: any,
  onEdit: (index: number, s: string) => void,
};

function getStartLineForIndex(chunks : Chunk[], index : number) {
  if (index === 0) { return 0; }

  return chunks[index - 1].startLine + chunks[index - 1].text.split('\n').length;
}

function mapStateToProps(state: State): stateProps {
  const {
    lintFailures,
    definitionsHighlights,
    currentFile,
    TMPchunks,
    TMPchunkIndexCounter,
  } = state;

  if (currentFile === undefined) {
    throw new Error('currentFile is undefined');
  }

  return {
    lintFailures,
    highlights: definitionsHighlights,
    name: currentFile,
    chunks: TMPchunks,
    chunkIndexCounter: TMPchunkIndexCounter,
  };
}

function mapDispatchToProps(dispatch: (action: Action) => any): dispatchProps {
  return {
    onEdit(index, contents) {
      dispatch({ type: 'updateChunkContents', index, contents });
    },
    doMagicA(
      chunks: Chunk[],
      chunkIndexCounter: number,
      index: number,
      text: string,
    ) {
      let newChunks : Chunk[];
      if (index === chunks.length) {
        const id = String(chunkIndexCounter);
        dispatch({ type: 'setChunkIndexCounter', chunkIndexCounter: chunkIndexCounter + 1 });
        newChunks = chunks.concat([{
          text,
          id,
          startLine: getStartLineForIndex(chunks, chunks.length),
        }]);
      } else {
        newChunks = chunks.map((p, ix) => {
          if (ix === index) { return { text, id: p.id, startLine: p.startLine }; }
          return p;
        });
        newChunks = newChunks.map((p, ix) => {
          if (ix <= index) { return p; }
          return {
            text: p.text,
            id: p.id,
            startLine: getStartLineForIndex(newChunks, ix),
          };
        });
      }
      dispatch({ type: 'setChunks', chunks: newChunks });
      dispatch({ type: 'updateChunkContents', index, contents: text });
    },
    doMagicB(result: DropResult, onEdit: (index: number, s: string) => void, chunks: Chunk[]) {
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
      onEdit(firstAffectedChunk, newChunks[firstAffectedChunk].text);
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunksProps = PropsFromRedux & dispatchProps & stateProps;

function DefChunks({
  doMagicA, doMagicB, chunks, chunkIndexCounter, name, lintFailures, highlights, onEdit,
}: DefChunksProps) {
  const onChunkEdit = (index: number, text: string) => {
    doMagicA(chunks, chunkIndexCounter, index, text);
  };
  const onDragEnd = (result: DropResult) => {
    if (result.destination !== null
        && result.source!.index !== result.destination!.index) {
      doMagicB(result, onEdit, chunks);
    }
  };

  function setupChunk(chunk: Chunk, index: number) {
    const linesInChunk = chunk.text.split('\n').length;
    let chunkHighlights : number[][];
    const chunkName = `${name}_chunk_${chunk.id}`;
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
    const isLast = index === chunks.length;
    return (
      <Draggable key={chunk.id} draggableId={chunk.id} index={index}>
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
                isLast={isLast}
                failures={failures}
                highlights={chunkHighlights}
                startLine={chunk.startLine}
                key={chunk.id}
                index={index}
                chunk={chunk.text}
                onEdit={onChunkEdit}
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
