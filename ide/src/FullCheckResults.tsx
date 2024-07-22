import React from 'react';
import { CheckResults } from './rhsObject';
import FailureComponent from './FailureComponent';
import { Failure } from './failure';
import { isInitializedEditor, UninitializedEditor } from './chunk';
import { CMEditor } from './utils';
import { RenderedCheckResultsAndSummary } from '../../src/runtime/checker';

type Props = {
  editor: UninitializedEditor | CMEditor 
  checks: RenderedCheckResultsAndSummary
};

export function FullCheckResults({
    editor,
    checks,
  }: Props) {

    const renderEditor = !isInitializedEditor(editor) ? undefined : editor;

    const blockResults = checks.renderedChecks.map((checkBlock) => {
        return <div>
            {checkBlock.testResults.map((test) => {
                return <FailureComponent editor={renderEditor} failure={test.rendered as Failure} />;
            })}
        </div>
    });

    return <p>
        {checks.checkSummary.message}: 
        {checks.checkSummary.passed} tests passed, 
        {checks.checkSummary.failed} tests failed,
        {checks.checkSummary.errored} blocks errored,
        {checks.checkSummary.total} total tests ran
        
        {blockResults}
    </p>
    
}