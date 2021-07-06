import React from 'react';
import { Failure } from './failure';
import { intersperse } from './utils';

export default function FailureComponent({ failure }: { failure: Failure }) {
  switch (failure.$name) {
    case 'paragraph':
      return (
        <>
          {failure.contents.map((f, i) => (
            // eslint-disable-next-line
            <FailureComponent failure={f} key={i} />
          ))}
        </>
      );
    case 'h-sequence':
      return (
        <>
          {intersperse(
            failure.contents.map((f, i) => (
            // eslint-disable-next-line
            <FailureComponent failure={f} key={i} />
            )),
            <>{failure.sep === '\n' ? '' : failure.sep}</>,
          )}
        </>
      );
    case 'text':
      return <>{failure.str}</>;
    case 'loc':
      return <>{failure.loc.asString}</>;
    case 'code':
      return <code><FailureComponent failure={failure.contents} /></code>;
    default:
      return <>{JSON.stringify(failure)}</>;
  }
}
