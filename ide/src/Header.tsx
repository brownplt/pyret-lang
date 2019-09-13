import React from 'react';

type HeaderProps = {};
type HeaderState = {};

export class Header extends React.Component<HeaderProps, HeaderState> {
    render() {
        return (
            <div className="header-container">
                {this.props.children}
            </div>
        );
    }
}
