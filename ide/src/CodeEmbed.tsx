import React from 'react';
import CM from 'codemirror';
import { UnControlled as UnControlledCM } from 'react-codemirror2';

type Props = {
  from: CM.Position,
  to: CM.Position,
  editor: CM.Editor & CM.Doc,
  text: string,
  failure: any;
};

type State = {};

export default class CodeEmbed extends React.Component<Props, State> {
  shouldComponentUpdate(nextProps: Props) {
    if (this.props.failure === nextProps.failure) { return false; }
    return true;
  }

  render() {
    const { text } = this.props;
    return (
      <UnControlledCM
        value={text}
        options={{ readOnly: true }}
        editorDidMount={(newEditor) => {
          newEditor.setSize(null, 'auto');
        }}
        className="failure-cmcode"
      />
    );
  }
}
