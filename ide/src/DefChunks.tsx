import React from 'react';
import { DragDropContext, Droppable, Draggable, DropResult } from "react-beautiful-dnd";
import { UnControlled as CodeMirror } from 'react-codemirror2';

type DefChunkProps = {
  index: number,
  chunk: string,
  onEdit: (key: number, chunk: string) => void
};
type DefChunkState = {};

export class DefChunk extends React.Component<DefChunkProps, DefChunkState> {
  render() {

    return (<div style={{ border: "1px solid #111", "paddingTop": "0.5em", "paddingBottom": "0.5em" }}>
      <CodeMirror
        editorDidMount={(editor, value) => {
          const cm = editor;
          editor.setSize(null, "auto");
        }}
        value={this.props.chunk}
        options={{
          mode: 'pyret',
          theme: 'default',
          lineNumbers: true,
          lineWrapping: true,
          lineNumberFormatter: (l) => String(l + this.props.index)
        }}
        onChange={(editor, __, value) => {
          return this.props.onEdit(this.props.index, value)
        }
        }
        autoCursor={false}>
      </CodeMirror></div>);

  }
}

type DefChunksProps = {
  chunks: string[],
  onEdit: (s: string) => void
};
type DefChunksState = {
  chunks: {id: string, text: string}[],
  chunkIndexCounter: number
};

export class DefChunks extends React.Component<DefChunksProps, DefChunksState> {
  constructor(props: DefChunksProps) {
    super(props);
    this.state = {
      chunkIndexCounter: this.props.chunks.length,
      chunks: this.props.chunks.map((text, id) => ({text, id: String(id)}))}
  }
  chunksToString(chunks : {id: string, text: string}[]) {
    return chunks.map((c) => c.text).join("\n");
  }
  render() {
    const onEdit = (index: number, text: string) => {
      let newChunks;
      if (index === this.state.chunks.length) {
        const id = String(this.state.chunkIndexCounter);
        this.setState({chunkIndexCounter: this.state.chunkIndexCounter + 1});
        newChunks = this.state.chunks.concat([{text, id}]);
      }
      else {
        newChunks = this.state.chunks.map((p, ix) => {
          if (ix === index) { return {text, id:p.id}; }
          else { return p; }
        });
      }
      this.setState({chunks: newChunks});
      this.props.onEdit(this.chunksToString(newChunks));
    }
    const onDragEnd = (result: DropResult) => {
      if(result.source!.index === result.destination!.index) {
        return;
      }
      else {
        // Great examples! https://codesandbox.io/s/k260nyxq9v
        const reorder = (chunks : {id: string, text: string}[], start : number, end : number) => {
          const result = Array.from(chunks);
          const [removed] = result.splice(start, 1);
          result.splice(end, 0, removed);
          return result;
        };
        if(result.destination === undefined) { return; }
        const newChunks = reorder(this.state.chunks, result.source.index, result.destination.index);
        this.setState({
          chunks: newChunks
        })
        this.props.onEdit(this.chunksToString(newChunks));
      }
    };
    return (<DragDropContext onDragEnd={onDragEnd}>
      <Droppable droppableId="droppable">
        {(provided, snapshot) => {
          return <div
            {...provided.droppableProps}
            ref={provided.innerRef} 
          >{this.state.chunks.concat([{text: "", id: String(this.state.chunkIndexCounter)}]).map((chunk, index) => {
            return <Draggable key={index} draggableId={String(index)} index={index}>
              {(provided, snapshot) => {
                return (<div ref={provided.innerRef}
                  {...provided.draggableProps}
                  {...provided.dragHandleProps}><DefChunk key={chunk.id} index={index} chunk={chunk.text} onEdit={onEdit}></DefChunk></div>)
              }
              }</Draggable>;
          })}</div>;
        }}
      </Droppable></DragDropContext>);
  }
}