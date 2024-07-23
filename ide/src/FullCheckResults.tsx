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
import { react } from '@babel/types';
import ActiveContext from './ActiveContext';


type Props = {
  frozen: boolean,
  editor: UninitializedEditor | CMEditor 
  checks: RenderedCheckResultsAndSummary
};

export function FullCheckResults({
    frozen,
    editor,
    checks,
  }: Props) {

    const renderEditor = !isInitializedEditor(editor) ? undefined : editor;

    const [activeCheck, setActive] = React.useState(0);
    const [editorContents, setEditorContents] = React.useState(editor.getValue());
    const [prevFrozen, setPrevFrozen] = React.useState(frozen);

    React.useEffect(() => {
        setPrevFrozen(frozen);
        if(frozen && !prevFrozen) {
            setEditorContents(editor.getValue());
        }
    }, [frozen]);

    const blockResults = checks.renderedChecks.map((checkBlock, i) => {
        return <AccordionItem uuid={i}>
                  <AccordionItemHeading>
                    <AccordionItemButton>
                        Details
                    </AccordionItemButton>
                  </AccordionItemHeading>
                  <AccordionItemPanel>
            {checkBlock.testResults.map((test) => {
                const failure = <FailureComponent editor={renderEditor} failure={test.rendered as Failure} />;
                return <ActiveContext.Provider value={i === activeCheck}>
                    {failure}
                </ActiveContext.Provider>;
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
        
        <Accordion 
            allowZeroExpanded
            allowMultipleExpanded={false}
            preExpanded={[0]}
            onChange={(ids => setActive(Number(ids[0])))}>
            {blockResults}
        </Accordion>
    </p>
    
}