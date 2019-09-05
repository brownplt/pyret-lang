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

  componentWillReceiveProps() {
    if(this.state.editor !== null) {
      const marks = this.state.editor.getDoc().getAllMarks();
      marks.forEach(m => m.clear());
    }
  }

  componentDidUpdate() {
    if(this.state.editor !== null) {
      const marks = this.state.editor.getDoc().getAllMarks();
      marks.forEach(m => m.clear());
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
          const marks = editor.getDoc().getAllMarks();
          marks.forEach(m => m.clear());
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

type Chunk = {
  startLine: number,
  id: string,
  text: string
}

type DefChunksProps = {
  highlights: number[][],
  interactErrorExists: boolean,
  program: string,
  name: string,
  onEdit: (s: string) => void
};
type DefChunksState = {
  chunks: Chunk[],
  chunkIndexCounter: number
};

const CHUNKSEP = "#.CHUNK#\n";

export class DefChunks extends React.Component<DefChunksProps, DefChunksState> {
  constructor(props: DefChunksProps) {
    super(props);
    const chunkstrs = this.props.program.split(CHUNKSEP);
    const chunks : Chunk[] = [];
    var totalLines = 0;
    for(let i = 0; i < chunkstrs.length; i += 1) {
      chunks.push({text: chunkstrs[i], id: String(i), startLine: totalLines})
      totalLines += chunkstrs[i].split("\n").length;
    }
    this.state = {
      chunkIndexCounter: chunkstrs.length,
      chunks
    }
  }
  getStartLineForIndex(chunks : Chunk[], index : number) {
    if(index === 0) { return 0; }
    else {
      return chunks[index - 1].startLine + chunks[index - 1].text.split("\n").length;
    }
  }
  chunksToString(chunks : Chunk[]) {
    return chunks.map((c) => c.text).join(CHUNKSEP);
  }
  render() {
    const onEdit = (index: number, text: string) => {
      let newChunks : Chunk[];
      if (index === this.state.chunks.length) {
        const id = String(this.state.chunkIndexCounter);
        this.setState({chunkIndexCounter: this.state.chunkIndexCounter + 1});
        newChunks = this.state.chunks.concat([{text, id, startLine: this.getStartLineForIndex(this.state.chunks, this.state.chunks.length)}])
      }
      else {
        newChunks = this.state.chunks.map((p, ix) => {
          if (ix === index) { return {text, id:p.id, startLine: p.startLine}; }
          else { return p; }
        });
        newChunks = newChunks.map((p, ix) => {
          if (ix <= index) { return p; }
          else { return {text: p.text, id: p.id, startLine: this.getStartLineForIndex(newChunks, ix)}; }
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
        const reorder = (chunks : Chunk[], start : number, end : number) => {
          const result = Array.from(chunks);
          const [removed] = result.splice(start, 1);
          result.splice(end, 0, removed);
          return result;
        };
        if(result.destination === undefined) { return; }
        let newChunks = reorder(this.state.chunks, result.source.index, result.destination.index);
        for(let i = 0; i < newChunks.length; i += 1) {
          const p = newChunks[i];
          newChunks[i] = {text: p.text, id: p.id, startLine: this.getStartLineForIndex(newChunks, i)};
        }
        this.setState({ chunks: newChunks });
        this.props.onEdit(this.chunksToString(newChunks));
      }
    };
    const endBlankChunk = {text: "", id: String(this.state.chunkIndexCounter), startLine: this.getStartLineForIndex(this.state.chunks, this.state.chunks.length)};
    return (<DragDropContext onDragEnd={onDragEnd}>
      <Droppable droppableId="droppable">
        {(provided, snapshot) => {
          return <div
            {...provided.droppableProps}
            ref={provided.innerRef} 
          >{this.state.chunks.concat([endBlankChunk]).map((chunk, index) => {
            const linesInChunk = chunk.text.split("\n").length;
            let highlights : number[][];
            if(this.props.interactErrorExists) {
              highlights = this.props.highlights.filter((h) => h[0] >= chunk.startLine && h[0] <= chunk.startLine + linesInChunk);
            }
            else {
              highlights = [];
            }
            return <Draggable key={index} draggableId={String(index)} index={index}>
              {(provided, snapshot) => {
                return (<div ref={provided.innerRef}
                  {...provided.draggableProps}
                  {...provided.dragHandleProps}><DefChunk highlights={highlights} startLine={chunk.startLine} key={chunk.id} index={index} chunk={chunk.text} onEdit={onEdit}></DefChunk></div>)
              }
              }</Draggable>;
          })}</div>;
        }}
      </Droppable></DragDropContext>);
  }
}