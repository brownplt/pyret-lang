import React from 'react';

type FooterProps = {
    message: string;
};
type FooterState = {};

export class Footer extends React.Component<FooterProps, FooterState> {
    render() {
        return (
            <div className="footer-container">
                {this.props.message}
            </div>
        );
    }
}
