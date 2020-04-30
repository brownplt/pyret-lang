import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Controlled as CodeMirror } from 'react-codemirror2';
import { State } from './state';
import { Chunk, getStartLineForIndex, newId } from './chunk';
import { Action } from './action';
import * as control from './control';

type stateProps = {
  chunks: Chunk[],
  focusedChunk: number | undefined,
};

function mapStateToProps(state: State): stateProps {
  const { chunks, focusedChunk } = state;
  return {
    chunks,
    focusedChunk,
  };
}

type propsFromReact = {
  index: number,
  onEdit: (key: number, chunk: string, shouldCreateNewChunk: boolean) => void,
  highlights: any,
  failures: any,
  name: string,
  focused: boolean,
};

type dispatchProps = {
  setFocusedChunk: (index: number) => void,
  setChunks: (chunks: Chunk[]) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): dispatchProps {
  return {
    setFocusedChunk(index: number) {
      dispatch({ type: 'update', key: 'focusedChunk', value: index });
    },
    setChunks(chunks: Chunk[]) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunkProps = PropsFromRedux & dispatchProps & stateProps & propsFromReact;

class DefChunk extends React.Component<DefChunkProps, any> {
  private input: React.RefObject<any>;

  constructor(props: DefChunkProps) {
    super(props);
    this.input = React.createRef();
  }

  // TODO (michael): investigate alternatives for this method
  // UNSAFE_componentWillReceiveProps() {
  //   const { chunks, index } = this.props;
  //   const { editor } = chunks[index];
  //   if (editor !== undefined) {
  //     const marks = editor.getDoc().getAllMarks();
  //     marks.forEach((m: any) => m.clear());
  //   }
  // }

  componentDidUpdate() {
    const {
    //  chunks,
      index,
    //  highlights,
    } = this.props;
    // const {
    //   editor,
    //   startLine,
    // } = chunks[index];
    // if (editor !== undefined) {
    //   const marks = editor.getDoc().getAllMarks();
    //   marks.forEach((m) => m.clear());
    //   if (highlights.length > 0) {
    //     for (let i = 0; i < highlights.length; i += 1) {
    //       editor.getDoc().markText(
    //         {
    //           line: highlights[i][0] - 1 - startLine,
    //           ch: highlights[i][1],
    //         },
    //         {
    //           line: highlights[i][2] - 1 - startLine,
    //           ch: highlights[i][3],
    //         },
    //         { className: 'styled-background-error' },
    //       );
    //     }
    //   }
    // }

    const { focusedChunk } = this.props;
    if (index === focusedChunk && this.input.current !== null) {
      console.log('FOCUSING ...');
      this.input.current.editor.focus();
    } else if (index === focusedChunk) {
      console.log('null errors eek');
    } else {
      console.log('total fail');
    }
  }

  scheduleUpdate(value: string) {
    const { chunks, index, setChunks } = this.props;

    const newChunks = [...chunks];
    newChunks[index] = {
      startLine: newChunks[index].startLine,
      text: value,
      id: newChunks[index].id,
    };
    for (let i = index; i < newChunks.length; i += 1) {
      newChunks[i] = {
        startLine: getStartLineForIndex(newChunks, i),
        text: newChunks[i].text,
        id: newChunks[i].id,
      };
    }
    setChunks(newChunks);
  }

  lint(value : string) {
    const { name } = this.props;
    control.lint(value, name);
  }

  handleArrowUp(editor: any, event: Event) {
    const { index, setFocusedChunk } = this.props;
    const pos = (editor as any).getCursor();
    if (pos.line === 0 && index > 0) {
      setFocusedChunk(index - 1);
      event.preventDefault();
    }
  }

  handleArrowDown(editor: any, event: Event) {
    const { index, setFocusedChunk, chunks } = this.props;
    const pos = (editor as any).getCursor();
    if (pos.line === chunks[index].text.split('\n').length - 1 && index < chunks.length - 1) {
      setFocusedChunk(index + 1);
      event.preventDefault();
    }
  }

  handleEnter(editor: any, event: Event) {
    const {
      chunks, index, setChunks, setFocusedChunk,
    } = this.props;
    const pos = (editor as any).getCursor();
    const token = editor.getTokenAt(pos);
    if (token.state.lineState.tokens.length === 0) {
      if (index + 1 === chunks.length) {
        const newChunks = [
          ...chunks.slice(),
          {
            text: '',
            startLine: getStartLineForIndex(chunks, index + 1),
            editor: undefined,
            id: newId(),
          },
        ];
        setChunks(newChunks);
        setFocusedChunk(index + 1);
        event.preventDefault();
      } else if (chunks[index + 1].text.trim() !== '') {
        const newChunks: Chunk[] = [
          ...chunks.slice(0, index + 1),
          {
            text: '',
            startLine: getStartLineForIndex(chunks, index + 1),
            id: newId(),
          },
          ...chunks.slice(index + 1),
        ];
        for (let i = index + 1; i < newChunks.length; i += 1) {
          newChunks[i] = {
            text: newChunks[i].text,
            startLine: getStartLineForIndex(newChunks, i),
            id: newChunks[i].id,
          };
        }
        setChunks(newChunks);
        setFocusedChunk(index + 1);
        event.preventDefault();
      } else if (chunks[index + 1].text.trim() === '') {
        setFocusedChunk(index + 1);
        event.preventDefault();
      }
    }
  }

  handleBackspace(event: Event) {
    const {
      chunks, index, setChunks, setFocusedChunk,
    } = this.props;
    if (index === 0 && chunks.length > 1 && chunks[0].text.trim() === '') {
      const newChunks = [...chunks.slice(1, chunks.length)];
      for (let i = 0; i < newChunks.length; i += 1) {
        newChunks[i] = {
          startLine: getStartLineForIndex(newChunks, i),
          text: newChunks[i].text,
          id: newChunks[i].id,
        };
      }
      setChunks(newChunks);
      setFocusedChunk(0);
      event.preventDefault();
    } else if (index > 0 && chunks[index].text.trim() === '') {
      const newChunks = [
        ...chunks.slice(0, index),
        ...chunks.slice(index + 1, chunks.length)];
      for (let i = index; i < newChunks.length; i += 1) {
        newChunks[i] = {
          startLine: getStartLineForIndex(newChunks, i),
          text: newChunks[i].text,
          id: newChunks[i].id,
        };
      }
      setChunks(newChunks);
      setFocusedChunk(index - 1);
      event.preventDefault();
    }
  }

  handleMouseDown() {
    const { index, setFocusedChunk } = this.props;
    setFocusedChunk(index);
  }

  render() {
    const {
      chunks, index, failures, focusedChunk,
    } = this.props;
    const { text, startLine } = chunks[index];
    const chunk = text;

    return (
      <div style={{
        width: '100%',
      }}
      >
        <CodeMirror
          ref={this.input}
          onMouseDown={() => {
            this.handleMouseDown();
          }}
          editorDidMount={(editor) => {
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
            autofocus: index === focusedChunk,
          }}
          onBeforeChange={(editor, data, value) => {
            this.scheduleUpdate(value);
          }}
          onKeyDown={(editor, event) => {
            switch ((event as any).key) {
              case 'Enter':
                this.handleEnter(editor, event);
                break;
              case 'Backspace':
                this.handleBackspace(event);
                break;
              case 'ArrowUp':
                this.handleArrowUp(editor, event);
                break;
              case 'ArrowDown':
                this.handleArrowDown(editor, event);
                break;
              default:
                console.log((event as any).key);
            }
          }}
          autoCursor
        />
        {failures.length !== 0 && (
          <ul>
            {failures.map((f: any, ix: number) => <li key={String(ix)}>{f}</li>)}
          </ul>
        )}
      </div>
    );
  }
}

export default connector(DefChunk);
