import React from 'react';

export type InteractionErrorProps = {
    fontSize: number,
};

export type InteractionErrorState = {};

export class InteractionError extends React.Component<InteractionErrorProps, InteractionErrorState> {
    render() {
        return (
            <div className="interaction-error">
                <p style={{ fontSize: this.props.fontSize }}>
                    {this.props.children}
                </p>
            </div>
        );
    }
}
