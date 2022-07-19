import React from 'react';
import Check from './reps/Check';
import RenderedValue from './reps/RenderedValue';
import {
  isLocation, isRHSCheck, isTrace, RHSObject,
} from './rhsObject';
import { NeverError } from './utils';

// .class, .class-selected
type Props = {
  isSelected: boolean,
  rhsObject: RHSObject,
  onMouseEnter?: () => void,
  className: string
  title?: string,
};

export default function RHSObjectComponent({
  isSelected, rhsObject, onMouseEnter, className, title,
}: Props) {
  const taggedClass = `${className} ${isSelected ? `${className}-selected` : ''}`;

  if (isTrace(rhsObject)) {
    // NOTE(luna): i don't think there are any undefined's we want to display, right?
    if (rhsObject.value === undefined) {
      return <div style={{ float: 'right' }} className="chatitor-rhs pending"> . . . </div>;
    }
    return (
      <pre
        className={taggedClass}
        onMouseEnter={onMouseEnter}
        title={title}
      >
        <RenderedValue value={rhsObject.value} />
      </pre>
    );
  }

  if (isLocation(rhsObject)) {
    return (
      <pre
        className={taggedClass}
        onMouseEnter={onMouseEnter}
        title={title}
      >
        {rhsObject.name}
        {' '}
        =
        {' '}
        <RenderedValue value={rhsObject.value} />
      </pre>
    );
  }

  if (isRHSCheck(rhsObject)) {
    return (
      <pre
        className={taggedClass}
        onMouseEnter={onMouseEnter}
        title={title}
      >
        <Check value={rhsObject} RenderedValue={RenderedValue} />
      </pre>
    );
  }

  throw new NeverError(rhsObject);
}
