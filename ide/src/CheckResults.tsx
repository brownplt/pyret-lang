import React from 'react';
import Check from './reps/Check';
import ExpandButton from './reps/ExpandButton';
import RenderedValue from './reps/RenderedValue';
import {
  RHSCheck,
} from './rhsObject';

type Props = {
  checks: RHSCheck[] | { testResults: RHSCheck[] }, // TODO: this isn't the right type
  className: string,
  title?: string,
};

export default function CheckResults({
  checks, className, title,
}: Props) {
  const actualChecks: RHSCheck[] = (checks as any)[0].testResults ?? checks;
  const [success, failed] = actualChecks.reduce(([s, f], check) => (
    ('$name' in check && check['$name'] == 'success') ? [s + 1, f] : [s, f + 1]
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
          <Check value={actualChecks[0]} RenderedValue={RenderedValue} />
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
  const details = expanded ? actualChecks.map((check, i) => <Check key={i} value={check} RenderedValue={RenderedValue} />) : '';
  const color = !expanded && !allPass ? '#fbbdaf' : '';
  return (
    <pre className={`chatitor-rhs ${className}`} style={{ backgroundColor: color, position: 'relative' }} title={title}>
      <ExpandButton expanded={expanded} setExpanded={setExpanded} showArrow />
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
