import React from 'react';
import { DragDropContext, Droppable, Draggable, DropResult } from "react-beautiful-dnd";
import { UnControlled as CodeMirror } from 'react-codemirror2';

type DefChunkProps = {
  highlights: number[][],
  index: number,
  startLine: number,
  chunk: string,
  onEdit: (key: number, chunk: string) => void
};
type DefChunkState = {
  editor: CodeMirror.Editor | null
};

export class DefChunk extends React.Component<DefChunkProps, DefChunkState> {
  constructor(props : DefChunkProps) { super(props); this.state = { editor: null }; }

  componentDidUpdate() {
    console.log("Updated a textarea: ", this.props.startLine, this.props, this.state);
    if(this.state.editor !== null) {
      for (let i = 0; i < this.state.editor.getDoc().getAllMarks().length; i++) {
          this.state.editor.getDoc().getAllMarks()[i].clear();
      }
      if (this.props.highlights.length > 0) {
          for (let i = 0; i < this.props.highlights.length; i++) {
              this.state.editor.getDoc().markText(
                  { line: this.props.highlights[i][0] - 1 - this.props.startLine,
                      ch: this.props.highlights[i][1] },
                  { line: this.props.highlights[i][2] - 1 - this.props.startLine,
                      ch: this.props.highlights[i][3] },
                  { className: "styled-background-error" });
          }
      }
    }
  }
  render() {
    return (<div style={{ border: "1px solid #111", "paddingTop": "0.5em", "paddingBottom": "0.5em" }}>
      <CodeMirror
        editorDidMount={(editor, value) => {
          this.setState({editor: editor});
          editor.setSize(null, "auto");
        }}
        value={this.props.chunk}
        options={{
          mode: 'pyret',
          theme: 'default',
          lineNumbers: true,
          lineWrapping: true,
          lineNumberFormatter: (l) => String(l + this.props.startLine)
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
  highlights: number[][],
  interactErrorExists: boolean,
  program: string,
  name: string,
  onEdit: (s: string) => void
};
type DefChunksState = {
  chunks: {id: string, text: string}[],
  chunkIndexCounter: number
};

const CHUNKSEP = "#.CHUNK#\n";

export class DefChunks extends React.Component<DefChunksProps, DefChunksState> {
  constructor(props: DefChunksProps) {
    super(props);
    const chunkstrs = this.props.program.split(CHUNKSEP);
    this.state = {
      chunkIndexCounter: chunkstrs.length,
      chunks: chunkstrs.map((text, id) => ({text, id: String(id)}))}
  }
  chunksToString(chunks : {id: string, text: string}[]) {
    return chunks.map((c) => c.text).join(CHUNKSEP);
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
      if(result.destination === null || result.source!.index === result.destination!.index) {
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
        this.setState({ chunks: newChunks });
        this.props.onEdit(this.chunksToString(newChunks));
      }
    };
    var startLine = 0;
    return (<DragDropContext onDragEnd={onDragEnd}>
      <Droppable droppableId="droppable">
        {(provided, snapshot) => {
          return <div
            {...provided.droppableProps}
            ref={provided.innerRef} 
          >{this.state.chunks.concat([{text: "", id: String(this.state.chunkIndexCounter)}]).map((chunk, index) => {
            const thisStartLine = startLine;
            startLine += chunk.text.split("\n").length;
            const highlights = this.props.highlights.filter((h) => h[0] >= thisStartLine && h[0] <= startLine);
            return <Draggable key={index} draggableId={String(index)} index={index}>
              {(provided, snapshot) => {
                return (<div ref={provided.innerRef}
                  {...provided.draggableProps}
                  {...provided.dragHandleProps}><DefChunk highlights={highlights} startLine={thisStartLine} key={chunk.id} index={index} chunk={chunk.text} onEdit={onEdit}></DefChunk></div>)
              }
              }</Draggable>;
          })}</div>;
        }}
      </Droppable></DragDropContext>);
  }
}