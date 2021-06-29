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
  };
  const selectedStyle = {
    background: isSelected ? '#d7d4f0' : '#efe',
    borderTop: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
    borderBottom: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
  };

  if (isTrace(rhsObject)) {
    return (
      <pre
        style={{
          ...style,
          ...selectedStyle,
        }}
        onMouseEnter={onMouseEnter}
      >
        <RenderedValue value={rhsObject.value} />
      </pre>
    );
  }

  if (isLocation(rhsObject)) {
    return (
      <pre
        style={{
          ...style,
          display: 'flex',
          alignItems: 'center',
          ...selectedStyle,
        }}
        onMouseEnter={onMouseEnter}
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
        style={{
          ...style,
          ...selectedStyle,
        }}
        onMouseEnter={onMouseEnter}
      >
        <RenderedValue value={rhsObject} />
      </pre>
    );
  }

  throw new NeverError(rhsObject);
}
