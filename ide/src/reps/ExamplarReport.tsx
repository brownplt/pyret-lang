
import React from 'react';

type ExamplarReportProps = {
    summaryString: any
};
type ExamplarReportState = {};

export default class ExamplarReportWidget extends React.Component<ExamplarReportProps, ExamplarReportState> {
  render() {
    const { summaryString } = this.props;
    return (
      <div>{summaryString}</div>
    );
  }
}
