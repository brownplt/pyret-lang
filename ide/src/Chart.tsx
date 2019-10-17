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
            // TODO(tiffany): max of horizontal axis needs to be max value
            hAxis: { title: this.props.headers[1],
                     viewWindow: { min: 0, max: 15 } },
            vAxis: { title: this.props.headers[0],
                     viewWindow: { min: 0, max: this.props.rows.length } },
            legend: "none"
        };
        const headers = [this.props.headers];
        const data = headers.concat(this.props.rows);
        console.log("data: ", data, "headers: ", this.props.headers, "rows: ", this.props.rows);
        return (
            <div>
                <Chart
                    chartType={this.props.chartType}
                    data={data}
                    options={options}
                    width="100%"
                    height="100%"
                    legendToggle
                />
            </div>
        );
    }
}
