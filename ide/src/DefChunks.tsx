import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import {
  DragDropContext, Droppable, Draggable, DropResult,
} from 'react-beautiful-dnd';
import { Action } from './action';
import {
  State,
} from './state';
import { Chunk, getStartLineForIndex } from './chunk';
import DefChunk from './DefChunk';

type stateProps = {
  chunks: Chunk[],
  focusedChunk: number | undefined,
};

type dispatchProps = {
  handleReorder: any,
};

function mapStateToProps(state: State): stateProps {
  const {
    chunks,
    focusedChunk,
  } = state;

  return {
    chunks,
    focusedChunk,
  };
}

function mapDispatchToProps(dispatch: (action: Action) => any): dispatchProps {
  return {
    handleReorder(
      result: DropResult,
      chunks: Chunk[],
      oldFocusedId: number | false,
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
        if (result.source.index < result.destination.index) {
          if (i >= result.source.index && i <= result.destination.index) {
            newChunks[i].errorState.status = 'notLinted';
          }
        } else if (result.source.index > result.destination.index) {
          if (i >= result.destination.index && i <= result.source.index) {
            newChunks[i].errorState.status = 'notLinted';
          }
        }
      }

      // const firstAffectedChunk = Math.min(result.source.index, result.destination.index);

      function getNewFocusedChunk() {
        for (let i = 0; i < newChunks.length; i += 1) {
          if (newChunks[i].id === oldFocusedId) {
            return i;
          }
        }

        return false;
      }

      dispatch({ type: 'update', key: 'chunks', value: newChunks });

      if (oldFocusedId !== false) {
        const newFocusedChunk = getNewFocusedChunk();
        if (newFocusedChunk === false) {
          throw new Error('handleReorder: new focused chunk is false');
        }

        dispatch({ type: 'update', key: 'focusedChunk', value: newFocusedChunk });
      }
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunksProps = PropsFromRedux & dispatchProps & stateProps;

function DefChunks({
  handleReorder,
  chunks,
  focusedChunk,
}: DefChunksProps) {
  const onDragEnd = (result: DropResult) => {
    if (result.destination !== null
        && result.source!.index !== result.destination!.index) {
      if (focusedChunk === undefined) {
        handleReorder(result, chunks, false);
      } else {
        const fc = chunks[focusedChunk];
        if (fc === undefined) {
          throw new Error('onDragEnd: chunks[focusedChunk] is undefined');
        }
        handleReorder(result, chunks, fc.id);
      }
    }
  };

  function setupChunk(chunk: Chunk, index: number) {
    const focused = focusedChunk === index;

    function getBorderColor() {
      if (focused && chunk.errorState.status === 'failed') {
        return 'red';
      }

      if (!focused && chunk.errorState.status === 'failed') {
        return '#ff9999';
      }

      if (focused && chunk.errorState.status === 'notLinted') {
        return 'orange';
      }

      if (!focused && chunk.errorState.status === 'notLinted') {
        return 'yellow';
      }

      if (focused) {
        return 'lightgray';
      }

      return '#eee';
    }

    const border = getBorderColor();

    return (
      <Draggable key={chunk.id} draggableId={String(chunk.id)} index={index}>
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
                  background: `${border}`,
                  borderRadius: '75% 0% 0% 75%',
                  marginLeft: '0.5em',
                }}
              >
                ::
              </div>
              <DefChunk
                key={chunk.id}
                index={index}
                focused={focused}
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
