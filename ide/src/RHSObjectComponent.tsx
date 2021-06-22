import React from 'react';
import RenderedValue from './reps/RenderedValue';
import {
  isLocation, isRHSCheck, isTrace, RHSObject,
} from './rhsObject';
import { NeverError } from './utils';

type Props = {isSelected: boolean, rhsObject: RHSObject, onMouseEnter?: () => void};

export default function RHSObjectComponent({ isSelected, rhsObject, onMouseEnter }: Props) {
  const selectedStyle = {
    background: isSelected ? '#d7d4f0' : 'rgba(0, 0, 0, 0)',
    borderTop: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
    borderBottom: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
  };

  if (isTrace(rhsObject)) {
    return (
      <pre
        style={{
          paddingLeft: '1em',
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
          display: 'flex',
          alignItems: 'center',
          paddingLeft: '1em',
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
          paddingLeft: '1em',
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
