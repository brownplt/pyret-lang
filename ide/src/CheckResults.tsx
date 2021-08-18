import React from 'react';
import ExpandButton from './reps/ExpandButton';
import RenderedValue from './reps/RenderedValue';
import {
  RHSCheck,
} from './rhsObject';

type Props = {
  checks: RHSCheck[],
  className: string,
  title?: string,
};

export default function RHSObjectComponent({
  checks, className, title,
}: Props) {
  const [success, failed] = checks.reduce(([s, f], check) => (
    check.success ? [s + 1, f] : [s, f + 1]
  ), [0, 0]);
  const allPass = failed === 0;
  const [expanded, setExpanded] = React.useState<boolean>(!allPass as boolean);
  const [lastAllPass, setLastAllPass] = React.useState<boolean>(allPass as boolean);
  // Auto-expand/collapse on fail/pass, but don't override user's choice
  if (lastAllPass !== allPass) {
    setExpanded(!allPass);
    setLastAllPass(allPass);
  }
  const successSummary = expanded ? <>Looks shipshape, all tests passed mate!</>
    : (
      <>
        <div style={{ display: 'inline-block', width: '90%' }}>
          <RenderedValue value={checks[0]} />
        </div>
        <br />
        ... and all
        {' '}
        {success - 1}
        {' '}
        others passed!
      </>
    );
  const summary = allPass
    ? successSummary
    : (
      <>
        {success}
        {' '}
        passed,
        {' '}
        {failed}
        {' '}
        failed
      </>
    );
  const details = expanded ? checks.map((check) => <RenderedValue value={check} />) : '';
  const color = !expanded && !allPass ? '#fbbdaf' : '';
  return (
    <pre className={`chatitor-rhs list-container ${className}`} style={{ backgroundColor: color }} title={title}>
      <ExpandButton expanded={expanded} setExpanded={setExpanded} />
      {' '}
      {summary}
      {' '}
      {details}
    </pre>
  );
}

/// associate with program text
/// red/green colorblindness
/// 50/50
/// background colors = no
