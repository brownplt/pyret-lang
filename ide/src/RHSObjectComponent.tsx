import React from 'react';
import RenderedValue from './reps/RenderedValue';
import {
  isCheckResults,
  isExamplarReport,
  isLocation, isTrace, RHSObject,
} from './rhsObject';
import { CMEditor, NeverError } from './utils';
import ExamplarReportWidget from './reps/ExamplarReport';
import { UninitializedEditor } from './chunk';

// .class, .class-selected
type Props = {
  isSelected: boolean,
  rhsObject: RHSObject,
  onMouseEnter?: () => void,
  className: string
  title?: string,
  editor: UninitializedEditor | CMEditor
};

export default function RHSObjectComponent({
  isSelected, rhsObject, onMouseEnter, className, title, editor
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

  if (isExamplarReport(rhsObject)) {
    return <ExamplarReportWidget editor={editor} wheatResults={rhsObject.wheatResults} chaffResults={rhsObject.chaffResults} hintMessage={rhsObject.hintMessage} qtmVariations={rhsObject.qtmVariations}/>
  }

  if (isCheckResults(rhsObject)) {
    return <p>Check results should not be rendered by RHSObjectComponent</p>
  }

  throw new NeverError(rhsObject);
}
