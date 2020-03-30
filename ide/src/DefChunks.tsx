import React from 'react';
import {
  DragDropContext, Droppable, Draggable, DropResult,
} from 'react-beautiful-dnd';
import { LintFailure, CHUNKSEP } from './state';
import DefChunk from './DefChunk';

type Chunk = {
  startLine: number,
  id: string,
  text: string
};

type DefChunksProps = {
  lintFailures: {[name : string]: LintFailure},
  highlights: number[][],
  chunks: string[],
  name: string,
  onEdit: (index: number, s: string) => void
};
type DefChunksState = {
  chunks: Chunk[],
  chunkIndexCounter: number
};

export default class DefChunks extends React.Component<DefChunksProps, DefChunksState> {
  static getStartLineForIndex(chunks : Chunk[], index : number) {
    if (index === 0) { return 0; }

    return chunks[index - 1].startLine + chunks[index - 1].text.split('\n').length;
  }

  static chunksToString(chunks : Chunk[]) {
    return chunks.map((c) => c.text).join(CHUNKSEP);
  }

  constructor(props: DefChunksProps) {
    super(props);
    const newChunks : Chunk[] = [];
    let totalLines = 0;
    const { chunks } = this.props;
    for (let i = 0; i < chunks.length; i += 1) {
      newChunks.push({ text: chunks[i], id: String(i), startLine: totalLines });
      totalLines += chunks[i].split('\n').length;
    }
    this.state = {
      chunkIndexCounter: chunks.length,
      chunks: newChunks,
    };
  }

  render() {
    const onChunkEdit = (index: number, text: string) => {
      let newChunks : Chunk[];
      const { chunks, chunkIndexCounter } = this.state;
      const { onEdit } = this.props;
      if (index === chunks.length) {
        const id = String(chunkIndexCounter);
        this.setState({ chunkIndexCounter: chunkIndexCounter + 1 });
        newChunks = chunks.concat([{
          text,
          id,
          startLine: this.getStartLineForIndex(chunks, chunks.length),
        }]);
      } else {
        newChunks = chunks.map((p, ix) => {
          if (ix === index) { return { text, id: p.id, startLine: p.startLine }; }
          return p;
        });
        newChunks = newChunks.map((p, ix) => {
          if (ix <= index) { return p; }
          return { text: p.text, id: p.id, startLine: this.getStartLineForIndex(newChunks, ix) };
        });
      }
      this.setState({ chunks: newChunks });
      onEdit(index, text);
    };
    const onDragEnd = (result: DropResult) => {
      if (result.destination !== null
          && result.source!.index !== result.destination!.index) {
        // Great examples! https://codesandbox.io/s/k260nyxq9v
        const reorder = (chunks : Chunk[], start : number, end : number) => {
          const newResult = Array.from(chunks);
          const [removed] = newResult.splice(start, 1);
          newResult.splice(end, 0, removed);
          return newResult;
        };
        if (result.destination === undefined) { return; }

        const { chunks } = this.state;
        const { onEdit } = this.props;

        const newChunks = reorder(chunks, result.source.index, result.destination.index);
        for (let i = 0; i < newChunks.length; i += 1) {
          const p = newChunks[i];
          newChunks[i] = {
            text: p.text,
            id: p.id,
            startLine: this.getStartLineForIndex(newChunks, i),
          };
        }
        this.setState({ chunks: newChunks });
        const firstAffectedChunk = Math.min(result.source.index, result.destination.index);
        onEdit(firstAffectedChunk, this.chunksToString(newChunks));
      }
    };

    const { chunkIndexCounter, chunks } = this.state;
    const { name, lintFailures, highlights } = this.props;
    const endBlankChunk = {
      text: '',
      id: String(chunkIndexCounter),
      startLine: this.getStartLineForIndex(chunks, chunks.length),
    };

    function setupChunk(chunk, index) {
      const linesInChunk = chunk.text.split('\n').length;
      let chunkHighlights : number[][];
      const chunkName = `${name}_chunk_${chunk.id}`;
      let failures : string[] = [];
      if (chunkName in lintFailures) {
        failures = lintFailures[chunkName].errors;
      }
      if (chunkHighlights.length > 0) {
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
              // eslint-disable-next-line react/jsx-props-no-spreading
              {...draggableProvided.dragHandleProps}
            >
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
          )}
        </Draggable>
      );
    }

    return (
      <DragDropContext onDragEnd={onDragEnd}>
        <Droppable droppableId="droppable">
          {(provided) => (
            <div
              // eslint-disable-next-line react/jsx-props-no-spreading
              {...provided.droppableProps}
              ref={provided.innerRef}
            >
              {chunks.concat([endBlankChunk]).map(setupChunk)}
            </div>
          )}
        </Droppable>
      </DragDropContext>
    );
  }
}
