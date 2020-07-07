import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import RenderedValue from './RenderedValue';
import { State } from './state';
import {
  RHSObjects,
  isTrace,
  isLocation,
  isRHSCheck,
} from './rhsObject';

type stateProps = {
  rhs: RHSObjects,
};

function mapStateToProps(state: State): stateProps {
  const { rhs } = state;
  return { rhs };
}

const connector = connect(mapStateToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type RHSProps = stateProps & PropsFromRedux;

function RHS({ rhs }: RHSProps) {
  const elements = (
    rhs.objects.map((rhsObject) => {
      if (isTrace(rhsObject)) {
        return (
          <pre key={rhsObject.key}>
            <RenderedValue value={rhsObject.value} />
          </pre>
        );
      }

      if (isLocation(rhsObject)) {
        return (
          <pre
            key={rhsObject.key}
            style={{
              display: 'flex',
              alignItems: 'center',
            }}
          >
            {rhsObject.name}
            {' '}
            =&nbsp;
            <RenderedValue value={rhsObject.value} />
          </pre>
        );
      }

      if (isRHSCheck(rhsObject)) {
        return (
          <pre key={rhsObject.key}>
            Test
            {' '}
            {rhsObject.success ? 'succeeded' : 'failed'}
            {' '}
            at
            {' '}
            {rhsObject.loc}
          </pre>
        );
      }

      throw new Error(`RHS: malformed RHSObject, ${JSON.stringify(rhsObject)}`);
    }));

  return (
    <div>
      {elements}
    </div>
  );
}

export default connector(RHS);
