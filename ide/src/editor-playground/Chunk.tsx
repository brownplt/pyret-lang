import React from 'react';
import CM from 'codemirror';
import Tooltip from './Tooltip';

type Props = {
  parentEditor: CodeMirror.Doc,
  start: number,
  end: number,
};
type State = {
  // editor: CodeMirror.Editor | 567,
  tooltip?: JSX.Element,
  showTooltip: boolean,
};

export default class Chunk extends React.Component<Props, State> {
  private tag: React.RefObject<HTMLDivElement>;

  constructor(props: Props) {
    super(props);
    this.tag = React.createRef();
    this.state = {
      showTooltip: false,
    };
  }

  componentDidMount() {
    if (this.tag.current === null) {
      throw new Error('ref didn\'t update');
    }
    const { parentEditor, start, end } = this.props;
    const doc = parentEditor.linkedDoc({
      sharedHist: true,
      from: start,
      to: end,
      // This might inherit, or it might be different. Could just specify 'pyret'
      mode: null,
    });
    const editor = CM(this.tag.current, {
      value: doc,
      mode: 'pyret',
      lineNumbers: true,
    });
    const clearTooltip = () => {
      this.setState({
        showTooltip: false,
      });
    };
    editor.on('mousedown', clearTooltip);
    editor.on('keydown', (ed: CM.Editor, event: Event) => {
      clearTooltip();
      if ((event as KeyboardEvent).key === 'Enter') {
        // Why isn't it in the type? idk
        const pos = (ed as any).getCursor();
        const token = ed.getTokenAt(pos);
        const line = doc.getLine(pos.line);
        if (line.replace(/ /g, '') !== '' && token.state.lineState.tokens.length === 0) {
          console.log('Presenting tooltip, next enter will chunk');
          // "What? This looks weird! Why are you doing pixel stuff!"
          // Well here are some examples of things that DON'T work:
          // - CM.markText: Seems perfect! But doesn't work at all on blank lines fsr!
          // - Modifying the text: Not a good abstraction, hard to keep the cursor
          //   in the right place, have to modify text back
          // - Grabbing the cursor itself and adding css after to it or something:
          //   fsr (React? CM?) it gets overwritten. It's also nigh impossible
          //   to tell cursors apart
          const lastLineBegin = { line: pos.line, ch: 0 };
          const { bottom, left } = ed.cursorCoords(lastLineBegin);
          this.setState({
            tooltip: <Tooltip top={bottom} left={left} />,
            showTooltip: true,
          });
        } else {
          console.log('First enter on unfinished block');
          // My design instinct is to show nothing here: in the happy case, the
          // person is just writing a multiline expression and having a ball
        }
      }
    });
  }

  render() {
    const { showTooltip } = this.state;
    // A combination of eslint (must destructure) and JS weirdness (destructure
    // null is TypeError) makes this a messier version of
    // const { showTooltip, tooltip } = ...
    let tooltip;
    if (showTooltip) {
      const { tooltip: grabbedTooltip } = this.state;
      tooltip = grabbedTooltip;
    }
    return (
      <>
        <div ref={this.tag} />
        {showTooltip ? tooltip : null}
      </>
    );
  }
}
