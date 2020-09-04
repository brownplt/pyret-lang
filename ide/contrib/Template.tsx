// This is a template file to create a chunk editor component.
// %s/TEMPORARY_NAME/yourComponentName
import React from 'react';

import {
  connect,
  ConnectedProps,
} from 'react-redux';

import {
  State,
} from './state';

type StateProps = {
};

function mapStateToProps(state: State): StateProps {
  const {
  } = state;

  return {
  };
}

type PropsFromReact = {
};

type DispatchProps = {
};

function mapDispatchToProps(dispatch: (action: Action) => void): DispatchProps {
  return {
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type Props = PropsFromRedux & DispatchProps & StateProps & PropsFromReact;

function TEMPORARY_NAME({}: Props) {
  return (
  );
}

export default connector(TEMPORARY_NAME);
