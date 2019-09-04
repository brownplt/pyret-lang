import React from 'react';
import {UnControlled as CodeMirror} from 'react-codemirror2';

type DefChunkProps = {
  index: number,
  chunk: string,
  onEdit: (key: number, chunk : string) => void
};
type DefChunkState = { };

export class DefChunk extends React.Component<DefChunkProps, DefChunkState> {
  render() {

    return (<div style={{border: "1px solid #111", "paddingTop": "0.5em", "paddingBottom": "0.5em"}}>
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
                }}
                onChange={(editor, __, value) => {
                  return this.props.onEdit(this.props.index, value)}
                }
                autoCursor={false}>
    </CodeMirror></div>);

  }
}

type DefChunksProps = {
  chunks: string[],
  onEdit: (s: string) => void
};
type DefChunksState = { };

export class DefChunks extends React.Component<DefChunksProps, DefChunksState> {
  constructor(props : DefChunksProps) {
    super(props);
  }
  render() {
    const onEdit = (index : number, value : string) => {
      let newChunks;
      if(index === this.props.chunks.length) {
        newChunks = this.props.chunks.concat([value]);
      }
      else {
        newChunks = this.props.chunks.map((p, ix) => {
          if(ix === index) { return value; }
          else { return p; }
        });
      }
      this.props.onEdit(newChunks.join("\n"))
    }
    return (<div>{this.props.chunks.concat([""]).map((chunk, index) => {
      return <DefChunk key={index} index={index} chunk={chunk} onEdit={onEdit}></DefChunk>})}</div>);
  }
}