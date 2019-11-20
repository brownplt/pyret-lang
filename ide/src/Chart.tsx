import React from 'react';
import Chart from 'react-google-charts';

type ChartWidgetProps = {
    headers: any[];
    rows: any[][];
    chartType: any;
};

type ChartWidgetState = {};

export class ChartWidget extends React.Component<ChartWidgetProps, ChartWidgetState> {
    render() {
        const options = {
            // TODO(tiffany): get title from the chart
            title: "Title",
            hAxis: { title: this.props.headers[1] },
            vAxis: { title: this.props.headers[0] },
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
