import React from 'react';
import { CheckResults } from './rhsObject';
import FailureComponent from './FailureComponent';
import { Failure } from './failure';
import { isInitializedEditor, UninitializedEditor } from './chunk';
import { CMEditor } from './utils';
import { Palette } from './palette';

type Props = {
  editor: UninitializedEditor | CMEditor 
  checks: CheckResults
};


export function FullCheckResults({
    editor,
    checks,
  }: Props) {

    const renderEditor = !isInitializedEditor(editor) ? undefined : editor;

    const blockResults = checks.renderedCheckBlockResults.renderedChecks.map((checkBlock) => {
        return <div>
            {checkBlock.testResults.map((test) => {
                return <FailureComponent editor={renderEditor} failure={test.rendered as Failure} />;
            })}
        </div>
    });

    return <p>
        {checks.renderedCheckBlockResults.checkSummary.message}: 
        {checks.renderedCheckBlockResults.checkSummary.passed} tests passed, 
        {checks.renderedCheckBlockResults.checkSummary.failed} tests failed,
        {checks.renderedCheckBlockResults.checkSummary.errored} blocks errored,
        {checks.renderedCheckBlockResults.checkSummary.total} total tests ran

        {blockResults}
    </p>
    
}