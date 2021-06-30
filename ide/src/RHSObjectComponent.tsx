import React from 'react';
import RenderedValue from './reps/RenderedValue';
import {
  isLocation, isRHSCheck, isTrace, RHSObject,
} from './rhsObject';
import { NeverError } from './utils';

type Props = {isSelected: boolean, rhsObject: RHSObject, onMouseEnter?: () => void};

export default function RHSObjectComponent({ isSelected, rhsObject, onMouseEnter }: Props) {
  const style = {
    paddingLeft: '1em',
    display: 'inline-block',
    border: '1px solid black',
    borderRadius: '0.5em',
    borderBottomRightRadius: '0',
    margin: '0',
    padding: '0.7em',
  };
  const wrapperStyle = {
    textAlign: 'right' as 'right', // ????
  };
  const selectedStyle = {
    background: isSelected ? '#d7d4f0' : 'rgba(0, 0, 0, 0)',
    // borderTop: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
    // borderBottom: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
  };

  if (isTrace(rhsObject)) {
    return (
      <div style={wrapperStyle}>
        <pre
          style={{
            ...style,
            ...selectedStyle,
          }}
          onMouseEnter={onMouseEnter}
        >
          <RenderedValue value={rhsObject.value} />
        </pre>
      </div>
    );
  }

  if (isLocation(rhsObject)) {
    return (
      <div style={wrapperStyle}>
        <pre
          style={{
            ...style,
            ...selectedStyle,
          }}
          onMouseEnter={onMouseEnter}
        >
          {rhsObject.name}
          {' '}
          =
          {' '}
          <RenderedValue value={rhsObject.value} />
        </pre>
      </div>
    );
  }

  if (isRHSCheck(rhsObject)) {
    return (
      <div style={wrapperStyle}>
        <pre
          style={{
            ...style,
            ...selectedStyle,
          }}
          onMouseEnter={onMouseEnter}
        >
          <RenderedValue value={rhsObject} />
        </pre>
      </div>
    );
  }

  throw new NeverError(rhsObject);
}
