import React from 'react';

export enum EMenu {
    FSBrowser,
    Options,
}

type MenuProps = {
    tabs: any[][],
    currentTab: number,
};
type MenuState = {};

export class Menu extends React.Component<MenuProps, MenuState> {
    render() {
        return (
            <div className="menu-content">
                {this.props.tabs[this.props.currentTab]}
            </div>
        );
    }
}
