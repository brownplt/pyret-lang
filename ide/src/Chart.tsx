import React from 'react';
import Chart from 'react-google-charts';

type ChartWidgetProps = {
  headers: any[];
  rows: any[][];
  chartType: any;
};

export default function ChartWidget({ headers, rows, chartType }: ChartWidgetProps) {
  const options = {
    // TODO(tiffany): get title from the chart
    title: 'Title',
    hAxis: { title: headers[1] },
    vAxis: { title: headers[0] },
    legend: 'none',
  };
  const headersArray = [headers];
  const data = headersArray.concat(rows);
  return (
    <div>
      <Chart
        chartType={chartType}
        data={data}
        options={options}
        width="100%"
        legendToggle
      />
    </div>
  );
}
