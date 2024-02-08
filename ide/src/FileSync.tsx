import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { ProjectState, State } from './state';
import * as action from './action';

type StateProps = {
  projectState: ProjectState,
};

function mapStateToProps(state: State): StateProps {
  return {
    projectState: state.projectState,
  };
}

type DispatchProps = {
  syncProject: () => void,
};

function mapDispatchToProps(dispatch: (action: action.Action) => any): DispatchProps {
  return {
    syncProject: () => dispatch({ type: 'fileSync' }),
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type FileSyncProps = PropsFromRedux & DispatchProps & StateProps;

type FileSyncState = {
  interval: number,
};

const SYNC_RATE = 3000;

class FileSync extends React.Component<FileSyncProps, FileSyncState> {
  componentDidMount() {
    if (this.props.projectState.type === 'gdrive') {
      const interval = setInterval(() => {
        this.props.syncProject();
      }, SYNC_RATE);
      this.setState({ interval });
    }
  }

  componentWillUnmount() {
    if (this.state) {
      clearInterval(this.state.interval);
    }
  }

  render() { return <></>; }
}

export default connector(FileSync);
