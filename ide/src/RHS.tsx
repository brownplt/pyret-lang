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
  fontSize: number,
};

function mapStateToProps(state: State): stateProps {
  const { rhs, fontSize } = state;
  return { rhs, fontSize };
}

const connector = connect(mapStateToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type RHSProps = stateProps & PropsFromRedux;

function RHS({ rhs, fontSize }: RHSProps) {
  const elements = (
    rhs.objects.map((rhsObject) => {
      if (isTrace(rhsObject)) {
        return (
          <pre
            key={rhsObject.key}
            style={{
              paddingLeft: '1em',
            }}
          >
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
              paddingLeft: '1em',
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
          <pre
            key={rhsObject.key}
            style={{
              paddingLeft: '1em',
            }}
          >
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

  const outdatedBackground = 'repeating-linear-gradient(45deg, #c8c8c8, #c8c8c8 8em, #979797 8em, #979797 16em)';

  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        background: rhs.outdated ? outdatedBackground : '#fff',
        fontSize,
        position: 'relative',
      }}
    >
      {elements}
    </div>
  );
}

export default connector(RHS);
