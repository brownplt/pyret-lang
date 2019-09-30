import React from 'react';
import {RenderedValue} from './RenderedValue';

export type Check = {
    lhs: any,
    rhs: any,
    path: string,
    loc: string,
    success: boolean
};

export type TestResultProps = {
  check: Check
};

export type TestResultState = {};
export class TestResult extends React.Component<TestResultProps, TestResultState> {
  render() {
    let message;
    const c = this.props.check;
    if(c.success) {
      message = <pre className="test-result">Test passed at {c.loc}</pre>;
    }
    else {
      const lhsRendered = <RenderedValue value={c.lhs}></RenderedValue>;
      const rhsRendered = <RenderedValue value={c.rhs}></RenderedValue>;
      message = <pre className="test-result">Test failed at {c.loc}: {lhsRendered} was not equal to {rhsRendered}</pre>;
    }
    return message;
  }
}
