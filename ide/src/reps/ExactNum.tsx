import React from 'react';
import ExpandButton from './ExpandButton';

export default function ExactNumWidget({
  num, den,
}: {num: number, den: number}) {
  const [expanded, setExpanded]: [boolean, (to: boolean) => void] = (
    // This should default to false like CPO does, but since we don't have
    // repeating decimal yet, the fraction is more helpful than an approximation
    React.useState(true as boolean)
  );
  return (
    <ExpandButton expanded={expanded} setExpanded={setExpanded} showArrow={false}>
      {/* TODO(luna): repeating decimal representation. CPO uses a library
      `jsnums` that is dependency injected in */}
      {expanded ? `${num}/${den}` : num / den}
    </ExpandButton>
  );
}
