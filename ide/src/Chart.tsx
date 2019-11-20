import React from 'react';
import Chart from 'react-google-charts';

type ChartWidgetProps = {
    headers: any[];
    rows: any;
    chartType: any;
};

type ChartWidgetState = {};

export class ChartWidget extends React.Component<ChartWidgetProps, ChartWidgetState> {
    render() {
        const options = {
            // TODO(tiffany): get title from the chart
            title: "Title",
            // TODO(tiffany): max of horizontal axis needs to be max value
            // TODO(tiffany): hTitle needed from props
            hAxis: { title: "hAxis Title",
                     viewWindow: { min: 0, max: 15 } },
            // TODO(tiffany): get max of vertical axis instead of num objs
            // TODO(tiffany): vTitle needed from props
            vAxis: { title: "vAxis Title",
                     viewWindow: { min: 0, max: 10 /*this.props.rows.length*/ } },
            legend: "none"
        };
        const headers = [this.props.headers];
        const data = headers.concat(this.props.rows);
        return (
            <div>
                <Chart
                    chartType={this.props.chartType}
                    data={data}
                    options={options}
                    width="100%"
                    legendToggle
                />
            </div>
        );
    }
}
