import React from 'react';
import RenderedValue from './RenderedValue';

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
    const { check } = this.props;
    if (check.success) {
      message = (
        <pre className="test-result">
          Test passed at
          {check.loc}
        </pre>
      );
    } else {
      const lhsRendered = <RenderedValue value={check.lhs} />;
      const rhsRendered = <RenderedValue value={check.rhs} />;
      message = (
        <pre className="test-result">
          Test failed at
          {check.loc}
          :
          {lhsRendered}
          {' '}
          was not equal to
          {rhsRendered}
        </pre>
      );
    }
    return message;
  }
}
