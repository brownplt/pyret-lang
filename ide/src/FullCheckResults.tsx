import React from 'react';
import FailureComponent from './FailureComponent';
import { Failure } from './failure';
import { isInitializedEditor, UninitializedEditor } from './chunk';
import { CMEditor } from './utils';
import { RenderedCheckResultsAndSummary } from '../../src/runtime/checker';
import {
    Accordion,
    AccordionItem,
    AccordionItemHeading,
    AccordionItemButton,
    AccordionItemPanel,
} from 'react-accessible-accordion';

// Demo styles, see 'Styles' section below for some notes on use.
import 'react-accessible-accordion/dist/fancy-example.css';


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
        return <AccordionItem>
                  <AccordionItemHeading>
                    <AccordionItemButton>
                        Details
                    </AccordionItemButton>
                  </AccordionItemHeading>
                  <AccordionItemPanel>
            {checkBlock.testResults.map((test) => {
                return <FailureComponent editor={renderEditor} failure={test.rendered as Failure} />;
            })}
                  </AccordionItemPanel>
                  </AccordionItem>
    });

    return <p>
        {checks.checkSummary.message}: 
        {checks.checkSummary.passed} tests passed, 
        {checks.checkSummary.failed} tests failed,
        {checks.checkSummary.errored} blocks errored,
        {checks.checkSummary.total} total tests ran
        
        <Accordion allowZeroExpanded>
            {blockResults}
        </Accordion>
    </p>
    
}