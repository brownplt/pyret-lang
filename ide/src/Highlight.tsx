import React from 'react';
import CM from 'codemirror';

interface State {
  mark?: CM.TextMarker;
}

type Props = {
  from: CM.Position,
  to: CM.Position,
  editor: CM.Editor & CM.Doc,
  color: string,
};

function posEq(a: CM.Position, b: CM.Position): boolean {
  return a.line === b.line && a.ch === b.ch;
}

export default class Highlight extends React.PureComponent<Props, State> {
  componentDidMount() {
    const {
      editor, from, to, color,
    } = this.props;
    this.setState({
      mark: editor.markText(from, to, { css: `background-color: ${color}` }),
    });
  }

  componentDidUpdate(oldProps: Props) {
    const {
      editor, from, to, color,
    } = this.props;
    const { mark } = this.state;
    if (mark === undefined) {
      throw new Error('mark should be created on mount');
    }
    if (!posEq(oldProps.from, from) || !posEq(oldProps.to, to)) {
      mark.clear();
      // eslint-disable-next-line
      this.setState({
        mark: editor.markText(from, to, { css: `background-color: ${color}` }),
      });
    } else {
      mark.changed();
    }
  }

  componentWillUnmount() {
    const { mark } = this.state;
    if (mark === undefined) {
      throw new Error('mark should be created on mount');
    }
    mark.clear();
  }

  render() {
    return <></>;
  }
}
