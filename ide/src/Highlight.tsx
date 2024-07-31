import React from 'react';
import CM from 'codemirror';
import HighlightsActiveContext from './HighlightsActiveContext';

interface State {
  mark?: CM.TextMarker;
  active: boolean;
}

type Props = {
  from: CM.Position,
  to: CM.Position,
  editor: CM.Editor,
  color: string,
};

function posEq(a: CM.Position, b: CM.Position): boolean {
  return a.line === b.line && a.ch === b.ch;
}

export default class Highlight extends React.PureComponent<Props, State> {
  constructor(props : Props) {
    super(props);
    this.state = { active: false };
  }
  componentDidMount() {
    const {
      editor, from, to, color,
    } = this.props;
    this.setState({
      active: false,
      mark: editor.getDoc().markText(from, to, { css: `background-color: ${color}` }),
    });
  }

  componentDidUpdate(oldProps: Props, oldState: State) {
    const {
      editor, from, to, color,
    } = this.props;
    const { mark } = this.state;
    if (mark === undefined) {
      throw new Error('mark should be created on mount');
    }
    if(!this.state.active) {
      mark.clear();
    }
    const becameActive = oldState.active === false && this.state.active === true;
    if (becameActive || !posEq(oldProps.from, from) || !posEq(oldProps.to, to) || oldProps.color !== color) {
      mark.clear();
      // eslint-disable-next-line
      this.setState({
        mark: editor.getDoc().markText(from, to, { css: `background-color: ${color}` }),
      });
      return;
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
    return <>
      <HighlightsActiveContext.Consumer>
        {active => this.setState({ active })}
      </HighlightsActiveContext.Consumer>
    </>
  }
}
