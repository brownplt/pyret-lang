import React, { useCallback } from 'react';
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
import ExpandedBlockContext from './ExpandedBlockContext';

import './FullCheckResults.css';
import HighlightsActiveContext from './HighlightsActiveContext';


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
        {errored > 0 && <SummaryBit count={errored} verb={"errored"} />}
      </div>
    </span>
  );
}

const TestResult = (props: {
  num: number,
  test: RenderedTestResult,
  active: boolean,
  setActive: () => void,
}) => {
  const { test, num, setActive } = props;
  const success = (test.$name === 'success');
  const className = success ? 'passing-test' : 'failing-test';
  const msg = success ? 'Passed' : 'Failed';
  return (
    <div onClick={setActive}>
      <div className={`check-block-test ${className}`}>
        <header>
          <a className="hinted-highlight">Test {num + 1}</a> - {msg}
        </header>
        {!success && <FailureComponent failure={test.rendered as Failure} />}
      </div>
    </div>
  );
}

export function CheckBlock({ testResults, expanded } : {
  testResults: RenderedTestResult[],
  expanded: boolean
}) {
  const [activeTest, setActiveTest] = React.useState(0);
  const testResultsRendered = testResults.map((test, testNum) => {
    const setActive = () => setActiveTest(testNum);
    const active = expanded && testNum === activeTest;
    return <ExpandedBlockContext.Provider value={expanded} key={testNum}>
      <HighlightsActiveContext.Provider value={active}>
        <TestResult num={testNum} test={test} active={active} setActive={setActive} />
      </HighlightsActiveContext.Provider>
    </ExpandedBlockContext.Provider>;
  });
  return <>{testResultsRendered}</>;
}

export function FullCheckResults({
  editor,
  checks,
}: Props) {

  const renderEditor = !isInitializedEditor(editor) ? undefined : editor;

  const [expandedCheckBlock, setExpandedCheckBlock] = React.useState(0);

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
      <AccordionItem key={i} uuid={i} className={`check-block ${blockClass}`}>
        <AccordionItemHeading>
          <AccordionItemButton>
            <span className="check-block-summary">{message}</span>
          </AccordionItemButton>
        </AccordionItemHeading>
        <AccordionItemPanel>
            <CheckBlock testResults={checkBlock.testResults} expanded={expandedCheckBlock === i} />
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
      onChange={(ids => setExpandedCheckBlock(Number(ids[0])))}>
      {blockResults}
    </Accordion>
  </div>
  
}