import React, { useLayoutEffect } from 'react';
import CM from 'codemirror';
import { UnControlled as UnControlledCM } from 'react-codemirror2';
import ActiveContext from './ActiveContext';

type Props = {
  firstLineNumber: number,
  text: string,
  failure: any,
};

type State = {
  editor: null | (CM.Editor),
};

export default class CodeEmbed extends React.Component<Props, State> {

  render() {
    const { text, firstLineNumber} = this.props;
    return (
      <div className="cm-snippet">
        <ActiveContext.Consumer>
          {active => {
            if(active && this.state) { this.state.editor?.refresh(); }
            return <></>;
          }}
        </ActiveContext.Consumer>
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
