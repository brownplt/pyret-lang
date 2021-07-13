import React from 'react';
import CM from 'codemirror';
import { Failure } from './failure';
import { intersperse } from './utils';
import Highlight from './Highlight';

type Props = { failure: Failure, editor: CM.Editor & CM.Doc };

export default function FailureComponent({ failure, editor }: Props) {
  switch (failure.$name) {
    case 'paragraph':
      return (
        <>
          {failure.contents.map((f, i) => (
            // eslint-disable-next-line
            <FailureComponent failure={f} key={i} editor={editor} />
          ))}
        </>
      );
    case 'h-sequence':
      return (
        <>
          {intersperse(
            failure.contents.map((f, i) => (
            // eslint-disable-next-line
            <FailureComponent failure={f} key={i} editor={editor} />
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
      return <code><FailureComponent failure={failure.contents} editor={editor} /></code>;
    case 'highlight': {
      const rainbow = ['#fcc', '#fca', '#cff', '#cfc', '#ccf', '#faf', '#fdf'];
      const color = rainbow[failure.color.valueOf() % rainbow.length];
      const locs = failure.locs.map((loc) => {
        if (loc.$name === 'builtin') {
          throw new Error('TODO(luna): builtin highlight somehow??');
        } else {
          const from = { line: loc['start-line'] - 1, ch: loc['start-column'] };
          const to = { line: loc['end-line'] - 1, ch: loc['end-column'] };
          return (
            <Highlight
              editor={editor}
              from={from}
              to={to}
              color={color}
              key={to.line * 13 + to.ch}
            />
          );
        }
      });
      return (
        <>
          <span style={{ backgroundColor: color }}>
            <FailureComponent
              failure={failure.contents}
              editor={editor}
              key={String(failure.color)}
            />
          </span>
          {locs}
        </>
      );
    }
    default:
      return <>{JSON.stringify(failure)}</>;
  }
}
