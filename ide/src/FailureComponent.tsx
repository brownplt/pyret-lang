import React from 'react';
import CM from 'codemirror';
import { connect } from 'react-redux';
import CodeEmbed from './CodeEmbed';
import { Failure } from './failure';
import { intersperse, srclocToCodeMirrorPosition } from './utils';
import Highlight from './Highlight';
import RenderedValue from './reps/RenderedValueWithOutput';
import { State } from './state';
import { Chunk, isInitializedEditor } from './chunk';
import { Palette } from './palette';

type WrapperProps = {
  failure: Failure,
  id?: string,
  editor?: CM.Editor & CM.Doc,
  highlightsOnly?: boolean,
};
type Props = WrapperProps & {
  palette: Palette,
};
type StateProps = {
  chunks: Chunk[],
};
function mapStateToProps(state: State): StateProps {
  const { chunks } = state;
  return { chunks };
}
const connector = connect(mapStateToProps, () => ({}));

function FailurePaletteWrapperUnconnected({
  failure, id, editor
}: WrapperProps) {
  const palette = React.useMemo(() => new Palette(), [failure]);
  return <FailureComponent palette={palette} failure={failure} id={id} editor={editor} />;
}

function FailureComponentUnconnected({
  failure, id, editor, chunks, palette
}: StateProps & Props) {
  switch (failure.$name) {
    case 'paragraph':
      return (
        <>
          {failure.contents.map((f, i) => (
            // eslint-disable-next-line react/no-array-index-key
            <FailureComponent palette={palette} failure={f} key={i} id={id} editor={editor} />
          ))}
        </>
      );
    case 'bulleted-sequence':
      return (
        <ul>
          {failure.contents.map((f, i) => (
            // eslint-disable-next-line react/no-array-index-key
            <li key={i}><FailureComponent palette={palette} failure={f} id={id} editor={editor} /></li>
          ))}
        </ul>
      );
    case 'h-sequence':
      return (
        <>
          {intersperse(
            failure.contents.map((f, i) => (
              // eslint-disable-next-line react/no-array-index-key
              <FailureComponent palette={palette} failure={f} key={i} id={id} editor={editor} />
            )),
            <>{failure.sep === '\n' ? '' : failure.sep}</>,
          )}
        </>
      );
    case 'h-sequence-sep':
      return (
        <>
          {intersperse(
            failure.contents.map((f, i) => (
              // eslint-disable-next-line react/no-array-index-key
              <FailureComponent palette={palette} failure={f} key={i} id={id} editor={editor} />
            )),
            <>{failure.sep === '\n' ? '' : failure.sep}</>,
            <>{failure.last}</>,
          )}
        </>
      );
    case 'embed':
      // A little strange as it seems to expect strings to be rendered as-is,
      // rather than indicated they are strings. That, in my opinion, is
      // completely wrong for a RHS, so we'll need to think about how properly
      // to special case this or if there are other mismatches between a
      // RenderedValue and an embed
      if (typeof failure.val === 'string') {
        return <>{failure.val}</>;
      }
      return <div><RenderedValue value={failure.val} /></div>;
    case 'text':
      return <>{failure.str}</>;
    case 'loc':
      if (failure.loc.$name === 'builtin') {
        return (
          <>
            builtin
            {' '}
            {failure.loc['module-name']}
          </>
        );
      }
      if (id && failure.loc.source.includes(id)) {
        return <>this message</>;
      }
      return (
        <>
          {failure.loc.source}
          :
          {failure.loc['start-line']}
        </>
      );
    case 'code':
      return <code><FailureComponent palette={palette} failure={failure.contents} id={id} editor={editor} /></code>;
    case 'cmcode': {
      if (failure.loc.$name !== 'srcloc') {
        throw new Error('Bad type of srcloc for a cmcode');
      }
      if (editor) {
        return (
          <CodeEmbed
            from={{ line: failure.loc['start-line'] - 1, ch: 0 }}
            to={{ line: failure.loc['end-line'], ch: 0 }}
            text={editor.getRange({ line: failure.loc['start-line'] - 1, ch: 0 }, { line: failure.loc['end-line'], ch: 0 })}
            editor={editor}
            failure={failure}
          />
        );
      }
      return <></>;
    }
    case 'highlight': {
      console.log('highlight: ', failure);
      const color = palette.getCSS(failure.color.valueOf());
      const calculated = failure.locs.map((loc) => {
        if (loc.$name === 'builtin' || failure.color.valueOf() === -1) {
          return undefined;
        }
        const { from, to } = srclocToCodeMirrorPosition(loc);
        const chunk = chunks.find((c) => loc.source.includes(c.id));
        if (chunk !== undefined && isInitializedEditor(chunk.editor)) {
          return { from, to, editor: chunk.editor };
        }
        return undefined;
      });
      const locs = calculated.map((attributes) => {
        if (attributes === undefined) {
          return <></>;
        }
        const { editor: ed, from, to } = attributes;
        return (
          <Highlight
            editor={ed}
            from={from}
            to={to}
            color={color}
            key={to.line * 13 + to.ch}
          />
        );
      });
      // It's only *now* that i realize it makes no sense for a single highlight
      // to have multiple locs... that would be two highlights so you can tell
      // them apart! Two highlights with the same loc, sure makes perfect sense.
      // So the data structure here is probably wrong... are there any of these?
      // For now i won't think hard about how to handle properly until it comes
      // up
      const focusRelevant = () => {
        // Scroll the one or zero (or more? in which case last) into view
        calculated.forEach((attributes) => {
          if (attributes === undefined) {
            return;
          }
          // We're def scrolling up
          attributes.editor.scrollIntoView(attributes.from);
        });
      };
      return (
        <>
          <button className="text-button" style={{ backgroundColor: color }} onClick={focusRelevant} type="button">
            <FailureComponent
              palette={palette} 
              failure={failure.contents}
              id={id}
              editor={editor}
              key={String(failure.color)}
            />
          </button>
          {locs}
        </>
      );
    }
    case 'maybe-stack-loc': {
      return <FailureComponent palette={palette} failure={failure['contents-without-loc']} id={id} editor={editor} />;
    }
    default:
      return <>{JSON.stringify(failure)}</>;
  }
}

const FailureComponent = connector(FailureComponentUnconnected);
const WrapperFailureComponent = connector(FailurePaletteWrapperUnconnected);
export default WrapperFailureComponent;
