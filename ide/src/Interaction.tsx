import React from 'react';
import {RenderedValue} from './RenderedValue';

type InteractionProps = {
    name: string;
    value: any;
};

type InteractionState = {};

export class Interaction extends React.Component<InteractionProps, InteractionState> {
    render() {
        if (this.props.name === "$checks" || this.props.name === "$answer") {
            return null;
        }

        return (
            <div className="interaction">
                {this.props.name !== "" ?
                <pre className="interaction-identifier">
                    {this.props.name} =&nbsp;
                </pre> : null}
                <RenderedValue value={this.props.value}></RenderedValue>
            </div>
        )
    };
}
