import React from 'react';
import { getAsyncRuntime } from '../runner';
import ExpandButton from './ExpandButton';

export default function NumWidget({ v }: { v: any }) {
  const { jsnums, $errCallbacks } = getAsyncRuntime();
  const [expanded, setExpanded] = React.useState<boolean>(true);

  if (jsnums.isInteger(v)) {
    return <span>{String(v)}</span>;
  } else if (jsnums.isRoughnum(v)) {
    return (
      <div className="roughnum">
        <span className="roughnum-start">~</span>
        <span>{String(v).slice(1)}</span>
      </div>
    );
  } else if (jsnums.isExact(v)) {
    const num = v.numerator();
    const den = v.denominator();
    const [prePoint, postPoint, repeat] = jsnums.toRepeatingDecimal(num, den, $errCallbacks);
    const expansion = (
      <span className="rationalNumber">
        {prePoint}
        <span className="point">.</span>
        {postPoint}
        { repeat !== '0' && <span className="rationalRepeat">{repeat}</span>}
      </span>
    );
    const collapsed = <span className="rationalNumber">{`${num}/${den}`}</span>;
    return (
      <ExpandButton expanded={expanded} setExpanded={setExpanded} showArrow={false}>
        {expanded ? expansion : collapsed }
      </ExpandButton>
    );
  } else {
    return (
      <span>
        Unknown numeric value:
        {JSON.stringify(v)}
      </span>
    );
  }
}
