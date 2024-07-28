import React from 'react';
import FailureComponent from './FailureComponent';
import { Failure } from './failure';
import { isInitializedEditor, UninitializedEditor } from './chunk';
import { CMEditor } from './utils';
import { RenderedCheckResultsAndSummary, RenderedTestResult } from '../../src/runtime/checker';
import {
  Accordion,
  AccordionItem,
  AccordionItemHeading,
  AccordionItemButton,
  AccordionItemPanel,
} from 'react-accessible-accordion';

// Demo styles, see 'Styles' section below for some notes on use.
import 'react-accessible-accordion/dist/fancy-example.css';
import ActiveContext from './ActiveContext';

import './FullCheckResults.css';


type Props = {
  editor: UninitializedEditor | CMEditor 
  checks: RenderedCheckResultsAndSummary,
};

const SummaryBit = (props: {
  count: number, verb: string, className?: string
}) => {
  const { count, verb, className = `summary-${verb}` } = props;
  return <div className={`summary-bit ${className}`}>
    <span className="summary-count">{count}</span>
    {count === 1 ? 'TEST' : 'TESTS'} {verb.toUpperCase()}
  </div>
}
const CheckBlockHeader = (props: {
  checkSummary: RenderedCheckResultsAndSummary['checkSummary']
}) => {
  const { checkSummary } = props;
  const { passed, failed, errored, total } = checkSummary;
  if (passed == total && errored === 0) {
    let counted: string;
    if (total === 1) { counted = "your test"; }
    else if (total === 2) { counted = "both tests"; }
    else { counted = `all ${total} tests`; }
    return (
      <span className="testing-summary check-block">
        {`Looks shipshape, ${counted} passed, mate!`}
      </span>
    )
  }
  return (
    <span className="testing-summary check-block">
      <div className="summary-bits">
        <SummaryBit count={passed} verb={"passed"} />
        <SummaryBit count={failed} verb={"failed"} />
        {errored > 0 &&  <SummaryBit count={errored} verb={"errored"} />}
      </div>
    </span>
  );
}

const TestResult = (props: {
  num: number,
  editor: CMEditor | undefined,
  test: RenderedTestResult,
}) => {
  const { editor, test, num } = props;
  if (test.$name === 'success') {
    return (
      <div className="check-block-test passing-test">
        <header>
          <a className="hinted-highlight">Test {num}</a> - Passed
        </header>
      </div>
    );
  } else {
    return (
      <div className="check-block-test failing-test">
        <header>
          <a className="hinted-highlight">Test {num}</a> - Failed
        </header>
        <FailureComponent editor={editor} failure={test.rendered as Failure} />
      </div>
    )
  }
}

export function FullCheckResults({
  editor,
  checks,
}: Props) {

  const renderEditor = !isInitializedEditor(editor) ? undefined : editor;

  const [activeCheck, setActive] = React.useState(0);

  const blockResults = checks.renderedChecks.map((checkBlock, i) => {
    const total = checkBlock.testResults.length;
    const passed = checkBlock.testResults.filter((t) => t.$name === 'success').length;
    const blockClass = `${passed == total ? 'check-block-success' : 'check-block-failed'}`
    let message: string;
    if (total === 1) {
      if (passed === total) {
        message = 'The test in this block passed.';
      } else {
        message = 'The test in this block failed.';
      }
    } else if (passed === total) {
      message = `All ${total} tests in this block passed.`;
    } else if (passed === 0) {
      message = `All ${total} tests in this block failed.`;
    } else {
      message = `${passed} out of ${total} tests passed in this block.`;
    }
    return (
      <AccordionItem uuid={i} className={`check-block ${blockClass}`}>
        <AccordionItemHeading>
          <AccordionItemButton>
            <span className="check-block-summary">{message}</span>
          </AccordionItemButton>
        </AccordionItemHeading>
        <AccordionItemPanel>
          {checkBlock.testResults.map((test, testNum) => {
            return <ActiveContext.Provider value={i === activeCheck} key={`${i}-${testNum}`}>
              <TestResult num={testNum} editor={renderEditor} test={test} />            
            </ActiveContext.Provider>;
          })}
        </AccordionItemPanel>
      </AccordionItem>
    )});

  if (checks.checkSummary.total === 0) { return null; }
  return <div className="check-block">
    <CheckBlockHeader checkSummary={checks.checkSummary} />
    <Accordion 
      allowZeroExpanded
      allowMultipleExpanded={false}
      preExpanded={[0]}
      onChange={(ids => setActive(Number(ids[0])))}>
      {blockResults}
    </Accordion>
  </div>
  
}