import React from 'react';

type SizerProps = {
    onIncrease: () => void,
    onDecrease: () => void,
    onReset: () => void,
    name: any,
};

type SizerState = {};

export class Sizer extends React.Component<SizerProps, SizerState> {
    render() {
        return (
            <div className="font-size-options">
                <button className="font-minus"
                        onClick={this.props.onDecrease}>
                    -
                </button>
                <button className="font-label"
                        onClick={this.props.onReset}>
                    {this.props.name}
                </button>
                <button className="font-plus"
                        onClick={this.props.onIncrease}>
                    +
                </button>
            </div>
        );
    }
}
