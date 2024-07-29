import React from 'react';
import CM from 'codemirror';
import { UnControlled as UnControlledCM } from 'react-codemirror2';
import ExpandedBlockContext from './ExpandedBlockContext';
import { Srcloc } from '../../src/runtime-arr/srcloc.arr';
import CodeMirror from 'codemirror';
import TimestampContext from './TimestampContext';
import { Variant } from '../../src/runtime/types/primitive-types';

type Props = {
  loc: Variant<Srcloc, 'srcloc'>,
  doc: CodeMirror.Doc,
};

type State = {
  editor: null | (CM.Editor),
  lastTimestamp: number
};

export default class CodeEmbed extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { lastTimestamp: -1, editor: null };
  }
  shouldComponentUpdate(nextProps: Readonly<Props>, nextState: Readonly<State>, nextContext: any): boolean {
    return this.state.lastTimestamp !== nextState.lastTimestamp;
  }
  render() {
    const { loc, doc } = this.props;
    const lastLineLength = doc.getLine(loc['end-line'] - 1).length;
    const firstLineNumber = loc['start-line'];
    const text = doc.getRange({ line: loc['start-line'] - 1, ch: 0 }, { line: loc['end-line'] - 1, ch: lastLineLength })
    return (
      <div className="cm-snippet">
        <ExpandedBlockContext.Consumer>
          {active => {
            if(active && this.state) { this.state.editor?.refresh(); }
            return <></>;
          }}
        </ExpandedBlockContext.Consumer>
        <TimestampContext.Consumer>
          {timestamp => {
            if(typeof timestamp === 'number' && timestamp !== this.state.lastTimestamp) {
              this.setState({ lastTimestamp: timestamp });
            }
            return <></>;
          }}
        </TimestampContext.Consumer>
        <UnControlledCM
          value={text}
          options={{
            readOnly:       "nocursor",
            indentUnit:     2,
            lineWrapping:   true,
            lineNumbers:    true,
            viewportMargin: 1,
            scrollbarStyle: "null",
            firstLineNumber: firstLineNumber
          }}
          editorDidMount={(newEditor) => {
            newEditor.setSize(null, 'auto');
            this.setState({ editor: newEditor });
          }}
          className="failure-cmcode"
        />

        
      </div>
    );
  }
}
