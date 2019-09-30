import React from 'react';
import { UnControlled as CodeMirror } from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import 'pyret-codemirror-mode/css/pyret.css';

// pyret-codemirror-mode/mode/pyret.js expects window.CodeMirror to exist and
// to be bound to the 'codemirror' import.
import * as RawCodeMirror from 'codemirror';
(window as any).CodeMirror = RawCodeMirror;
require('pyret-codemirror-mode/mode/pyret');


type SingleCodeMirrorDefinitionsProps = {
  onEdit: (s: string) => void,
  highlights: number[][],
  text: string
};
type SingleCodeMirrorDefinitionsState = {
  editor: CodeMirror.Editor | null
};

export class SingleCodeMirrorDefinitions extends React.Component<SingleCodeMirrorDefinitionsProps, SingleCodeMirrorDefinitionsState> {

  constructor(props : SingleCodeMirrorDefinitionsProps) { super(props); this.state = { editor: null }; }

  componentDidUpdate() {
    if(this.state.editor !== null) {
      if (this.props.highlights.length > 0) {
          for (let i = 0; i < this.props.highlights.length; i++) {
              this.state.editor.getDoc().markText(
                  { line: this.props.highlights[i][0] - 1,
                      ch: this.props.highlights[i][1] },
                  { line: this.props.highlights[i][2] - 1,
                      ch: this.props.highlights[i][3] },
                  { className: "styled-background-error" });
          }
      } else {
          const marks = this.state.editor.getDoc().getAllMarks();
          for (let i = 0; i < marks.length; i++) {
              marks[i].clear();
          }
      }
    }
  }
  onChange = (editor: CodeMirror.Editor, data: CodeMirror.EditorChange, value: string): void => {
    this.setState({ editor: editor });

    for ( var i = 0; i < editor.getDoc().getAllMarks().length; i++) {
        editor.getDoc().getAllMarks()[i].clear();
    }
    this.props.onEdit(value);
  }


  render() {
    return (<CodeMirror value={this.props.text}
      options={{
        mode: 'pyret',
        theme: 'default',
        lineNumbers: true,
        lineWrapping: true,
      }}
      onChange={this.onChange}
      autoCursor={false}>
    </CodeMirror>)
  }
}

