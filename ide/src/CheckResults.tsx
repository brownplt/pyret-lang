import React from 'react';
import ExpandButton from './reps/ExpandButton';
import RenderedValue from './reps/RenderedValue';
import {
  RHSCheck,
} from './rhsObject';

type Props = {
  checks: RHSCheck[],
  outdated?: boolean,
};

export default function RHSObjectComponent({
  checks, outdated,
}: Props) {
  const [success, failed] = checks.reduce(([s, f], check) => (
    check.success ? [s + 1, f] : [s, f + 1]
  ), [0, 0]);
  const [expanded, setExpanded]: [boolean, (to: boolean) => void] = (
    React.useState((failed !== 0) as boolean)
  );
  const summary = failed === 0 ? 'Looks shipshape, all tests passed, mate!'
    : `${success} passed, ${failed} failed`;
  const details = expanded ? checks.map((check) => <RenderedValue value={check} />) : '';
  return (
    <pre className={`chatitor-rhs list-container${outdated ? ' outdated' : ''}`}>
      <ExpandButton expanded={expanded} setExpanded={setExpanded} />
      {' '}
      {summary}
      {' '}
      {details}
    </pre>
  );
}
