import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Controlled as CodeMirror } from 'react-codemirror2';
import { State } from './state';
import { Chunk, getStartLineForIndex, newId } from './chunk';
import { Action } from './action';

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
  focused: boolean,
};

type dispatchProps = {
  setFocusedChunk: (index: number) => void,
  setChunks: (chunks: Chunk[]) => void,
  setChunk: (chunk: Chunk) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): dispatchProps {
  return {
    setFocusedChunk(index: number) {
      dispatch({ type: 'update', key: 'focusedChunk', value: index });
    },
    setChunks(chunks: Chunk[]) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    setChunk(chunk: Chunk) {
      dispatch({ type: 'update', key: 'chunks', value: chunk });
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
      chunks,
      index,
    } = this.props;

    const {
      editor,
      errorState,
      startLine,
    } = chunks[index];
    if (editor && errorState.status === 'succeeded') {
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
    } else if (editor && errorState.status === 'failed') {
      const { highlights } = errorState;
      const marks = editor.getDoc().getAllMarks();
      marks.forEach((m) => m.clear());
      if (highlights.length > 0) {
        for (let i = 0; i < highlights.length; i += 1) {
          const doc = editor.getDoc();
          // lint errors are relative to the start of a chunk, compile errors
          // are relative to the start of the program
          if (errorState.effect === 'lint') {
            const [l1, ch1, l2, ch2] = highlights[i];
            doc.markText(
              {
                line: l1 - 1,
                ch: ch1,
              },
              {
                line: l2 - 1,
                ch: ch2,
              },
              { className: 'styled-background-error' },
            );
          } else if (errorState.effect === 'compile') {
            const [l1, ch1, l2, ch2] = highlights[i];
            doc.markText(
              {
                line: l1 - startLine - 1,
                ch: ch1,
              },
              {
                line: l2 - startLine - 1,
                ch: ch2,
              },
              { className: 'styled-background-error' },
            );
          }
        }
      }
    }

    const { focusedChunk } = this.props;
    if (index === focusedChunk && this.input.current !== null) {
      this.input.current.editor.focus();
    }
  }

  scheduleUpdate(value: string) {
    const { chunks, index, setChunks } = this.props;

    const newChunks = [...chunks];
    newChunks[index] = {
      ...newChunks[index],
      text: value,
      errorState: { status: 'notLinted' },
    };
    for (let i = index; i < newChunks.length; i += 1) {
      newChunks[i] = {
        ...newChunks[i],
        startLine: getStartLineForIndex(newChunks, i),
      };
    }
    setChunks(newChunks);
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
        const newChunks: Chunk[] = [
          ...chunks.slice(),
          {
            text: '',
            startLine: getStartLineForIndex(chunks, index + 1),
            id: newId(),
            errorState: { status: 'notLinted' },
            editor: false,
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
            errorState: { status: 'notLinted' },
            editor: false,
          },
          ...chunks.slice(index + 1),
        ];
        for (let i = index + 1; i < newChunks.length; i += 1) {
          newChunks[i] = {
            ...newChunks[i],
            startLine: getStartLineForIndex(newChunks, i),
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
          ...newChunks[i],
          startLine: getStartLineForIndex(newChunks, i),
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
          ...newChunks[i],
          startLine: getStartLineForIndex(newChunks, i),
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
      chunks, index, focusedChunk,
    } = this.props;
    const { text, startLine } = chunks[index];

    return (
      <div style={{
        width: '100%',
        display: 'flex',
      }}
      >
        <CodeMirror
          ref={this.input}
          onMouseDown={() => {
            this.handleMouseDown();
          }}
          editorDidMount={(editor) => {
            const { setChunk } = this.props;

            const marks = editor.getDoc().getAllMarks();
            marks.forEach((m) => m.clear());
            editor.setSize(null, 'auto');

            setChunk({ ...chunks[index], editor });
          }}
          value={text}
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
            }
          }}
          autoCursor
        />
        {(() => {
          const chunk = chunks[index];

          if (chunk.errorState.status === 'failed') {
            return (
              <div style={{
                alignSelf: 'center',
                background: '#FFF2F2',
                position: 'sticky',
                top: '2.7em',
                width: '50%',
                zIndex: 1000,
                fontFamily: 'sans-serif',
                borderRadius: '3px',
                border: '0.3em solid hsl(204, 100%, 74%)',
                padding: '0.2em',
                marginRight: '1em',
                boxShadow: '0 0 1em',
              }}
              >
                {chunk.errorState.failures}
              </div>
            );
          }

          return false;
        })()}
      </div>
    );
  }
}

export default connector(DefChunk);
