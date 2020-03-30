import React from 'react';
import { UnControlled as CodeMirror } from 'react-codemirror2';
import * as control from './control';

type DefChunkProps = {
  name: string,
  failures: string[],
  highlights: number[][],
  index: number,
  startLine: number,
  chunk: string,
  onEdit: (key: number, chunk: string) => void,
  isLast: boolean
};
type DefChunkState = {
  editor: CodeMirror.Editor | null,
  focused: boolean,
  updateTimer: NodeJS.Timeout
};

export default class DefChunk extends React.Component<DefChunkProps, DefChunkState> {
  constructor(props : DefChunkProps) {
    super(props);
    const onFirstUpdate = () => {
      const { chunk } = this.props;
      this.lint(chunk);
    };
    this.state = { editor: null, updateTimer: setTimeout(onFirstUpdate, 0), focused: false };
  }

  // TODO (michael): investigate alternatives for this method
  UNSAFE_componentWillReceiveProps() {
    const { editor } = this.state;
    if (editor !== null) {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    }
  }

  componentDidUpdate() {
    const { editor } = this.state;
    const { highlights, startLine } = this.props;
    if (editor !== null) {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
      if (highlights.length > 0) {
        for (let i = 0; i < highlights.length; i += 1) {
          editor.getDoc().markText(
            {
              line: highlights[i][0] - 1 - startLine,
              ch: highlights[i][1],
            },
            {
              line: highlights[i][2] - 1 - startLine,
              ch: highlights[i][3],
            },
            { className: 'styled-background-error' },
          );
        }
      }
    }
  }

  scheduleUpdate(value : string) {
    const { updateTimer } = this.state;
    const { onEdit, index } = this.props;
    clearTimeout(updateTimer);
    this.setState({
      updateTimer: setTimeout(() => {
        onEdit(index, value);
        this.lint(value);
      }, 250),
    });
  }

  lint(value : string) {
    const { name } = this.props;
    control.lint(value, name);
  }

  render() {
    let borderWidth = '1px';
    let borderColor = '#eee';
    let shadow = '';
    const { focused } = this.state;
    const {
      highlights, isLast, onEdit, index, chunk, startLine, failures,
    } = this.props;
    if (focused) { shadow = '3px 3px 2px #aaa'; borderWidth = '2px'; borderColor = 'black'; }
    if (highlights.length > 0) { borderColor = 'red'; }
    const border = `${borderWidth} solid ${borderColor}`;
    return (
      <div style={{
        boxShadow: shadow, border, paddingTop: '0.5em', paddingBottom: '0.5em',
      }}
      >
        <CodeMirror
          onFocus={() => {
            if (isLast) {
              onEdit(index, '');
            }
            this.setState({ focused: true });
          }}
          onBlur={() => this.setState({ focused: false })}
          editorDidMount={(editor) => {
            this.setState({ editor });
            const marks = editor.getDoc().getAllMarks();
            marks.forEach((m) => m.clear());
            editor.setSize(null, 'auto');
          }}
          value={chunk}
          options={{
            mode: 'pyret',
            theme: 'default',
            lineNumbers: true,
            lineWrapping: true,
            lineNumberFormatter: (l) => String(l + startLine),
          }}
          onChange={(editor, __, value) => {
            this.scheduleUpdate(value);
          }}
          autoCursor={false}
        />
        <ul>
          {failures.map((f, ix) => <li key={String(ix)}>{f}</li>)}
        </ul>
      </div>
    );
  }
}
