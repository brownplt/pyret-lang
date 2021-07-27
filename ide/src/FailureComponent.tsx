import React from 'react';
import CM from 'codemirror';
import { UnControlled as UnControlledCM } from 'react-codemirror2';
import { Failure } from './failure';
import { intersperse } from './utils';
import Highlight from './Highlight';
import RenderedValue from './reps/RenderedValue';

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
    case 'embed':
      // A little strange as it seems to expect strings to be rendered as-is,
      // rather than indicated they are strings. That, in my opinion, is
      // completely wrong for a RHS, so we'll need to think about how properly
      // to special case this or if there are other mismatches between a
      // RenderedValue and an embed
      return <RenderedValue value={failure.val} />;
    case 'text':
      return <>{failure.str}</>;
    case 'loc':
      return <>{failure.loc.asString}</>;
    case 'code':
      return <code><FailureComponent failure={failure.contents} editor={editor} /></code>;
    case 'cmcode': {
      if (failure.loc.$name !== 'srcloc') {
        throw new Error('Bad type of srcloc for a cmcode');
      }
      return (
        <UnControlledCM
          value={editor.getRange({ line: failure.loc['start-line'] - 1, ch: 0 }, { line: failure.loc['end-line'], ch: 999999 })}
          options={{ readOnly: true }}
          editorDidMount={(newEditor) => {
            newEditor.setSize(null, 'auto');
          }}
          className="failure-cmcode"
        />
      );
    }
    case 'highlight': {
      const rainbow = ['#fcc', '#fca', '#cff', '#cfc', '#ccf', '#faf', '#fdf'];
      const color = rainbow[failure.color.valueOf() % rainbow.length];
      const locs = failure.locs.map((loc) => {
        if (loc.$name === 'builtin') {
          return <></>;
        }
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
