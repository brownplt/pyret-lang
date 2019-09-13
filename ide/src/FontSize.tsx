import React from 'react';

type FontSizeProps = {
    onIncrease: () => void,
    onDecrease: () => void,
    onReset: () => void,
    size: number,
};

type FontSizeState = {};

export class FontSize extends React.Component<FontSizeProps, FontSizeState> {
    render() {
        return (
            <div className="font-size-options">
                <button className="font-minus"
                        onClick={this.props.onDecrease}>
                    -
                </button>
                <button className="font-label"
                        onClick={this.props.onReset}>
                    Font ({this.props.size} px)
                </button>
                <button className="font-plus"
                        onClick={this.props.onIncrease}>
                    +
                </button>
            </div>
        );
    }
}
