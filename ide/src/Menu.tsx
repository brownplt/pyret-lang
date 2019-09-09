import React from 'react';

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
