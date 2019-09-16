import React from 'react';

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
      message = `Test succeeded at ${c.loc}`
    }
    else {
      message = `Test failed at ${c.loc}: ${c.lhs} was not equal to ${c.rhs}`
    }
    return <pre>{message}</pre>;
  }
}
