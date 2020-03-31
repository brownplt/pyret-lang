import React from 'react';
import { UnControlled as CodeMirror } from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import 'pyret-codemirror-mode/css/pyret.css';

// pyret-codemirror-mode/mode/pyret.js expects window.CodeMirror to exist and
// to be bound to the 'codemirror' import.
import * as RawCodeMirror from 'codemirror';

(window as any).CodeMirror = RawCodeMirror;
require('pyret-codemirror-mode/mode/pyret');


type Props = {
  onEdit: (s: string) => void,
  highlights: number[][],
  text: string
};
type State = {
  editor: CodeMirror.Editor | null
};

export default class SingleCodeMirrorDefinitions extends React.Component<Props, State> {
  constructor(props : Props) {
    super(props); this.state = { editor: null };
  }

  componentDidUpdate() {
    const { editor } = this.state;
    const { highlights } = this.props;

    if (editor !== null) {
      if (highlights.length > 0) {
        for (let i = 0; i < highlights.length; i += 1) {
          editor.getDoc().markText(
            {
              line: highlights[i][0] - 1,
              ch: highlights[i][1],
            },
            {
              line: highlights[i][2] - 1,
              ch: highlights[i][3],
            },
            { className: 'styled-background-error' },
          );
        }
      } else {
        const marks = editor.getDoc().getAllMarks();
        for (let i = 0; i < marks.length; i += 1) {
          marks[i].clear();
        }
      }
    }
  }

  onChange = (editor: CodeMirror.Editor, data: CodeMirror.EditorChange, value: string): void => {
    const { onEdit } = this.props;

    this.setState({ editor });

    for (let i = 0; i < editor.getDoc().getAllMarks().length; i += 1) {
      editor.getDoc().getAllMarks()[i].clear();
    }
    onEdit(value);
  };


  render() {
    const { text } = this.props;

    return (
      <CodeMirror
        value={text}
        options={{
          mode: 'pyret',
          theme: 'default',
          lineNumbers: true,
          lineWrapping: true,
        }}
        onChange={this.onChange}
        autoCursor={false}
      />
    );
  }
}
