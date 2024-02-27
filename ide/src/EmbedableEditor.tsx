import React from 'react';
import './App.css';
import { connect, ConnectedProps } from 'react-redux';
import { Chunk } from './chunk';
import * as State from './state';
import Footer from './Footer';
import Header from './Header';
import Run from './Run';
import * as action from './action';
import Chatitor from './Chatitor';

type StateProps = {
  browseRoot: string,
  browsePath: string,
  currentFileContents: undefined | string,
  fontSize: number,
  chunks: Chunk[],
};

function mapStateToProps(state: State.State): StateProps {
  return {
    browseRoot: state.browseRoot,
    browsePath: state.browsePath,
    currentFileContents: state.definitionsEditor.getValue(),
    fontSize: state.fontSize,
    chunks: state.chunks,
  };
}

type DispatchProps = {
  runProgram: () => void,
  update: (kv : Partial<State.State>) => void,
  loadFile: () => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): DispatchProps {
  return {
    runProgram: () => dispatch({ type: 'run' }),
    update: (kv) => dispatch({ type: 'update', key: 'updater', value: (s : State.State) => ({ ...s, ...kv }) }),
    loadFile() {
      dispatch({ type: 'enqueueEffect', effect: { effectKey: 'loadFile' } });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;

type EmbedableEditorProps = PropsFromRedux & DispatchProps & StateProps;

class EmbedableEditor extends React.Component<EmbedableEditorProps, any> {
  constructor(props : EmbedableEditorProps) {
    super(props);
    this.props.loadFile();
  }

  render() {
    const { fontSize } = this.props;
    const definitions = <Chatitor />;
    const mainContent = (
      <div className="edit-area-container" style={{ fontSize, width: '100%' }}>
        {definitions}
      </div>
    );
    return (
      <div className="page-container">
        <Header>
          <Run />
        </Header>
        <div className="code-container">
          {mainContent}
        </div>
        <Footer />
      </div>
    );
  }
}

export default connector(EmbedableEditor);
