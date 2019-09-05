import React from 'react';

export enum EMenu {
    FSBrowser,
    Options,
}

type MenuProps = {
    menu: EMenu,
    fsContent: any,
    menuContent: any[],
};
type MenuState = {};

export class Menu extends React.Component<MenuProps, MenuState> {
    render() {
        if (this.props.menu === EMenu.FSBrowser) {
            return this.props.fsContent;
        } else if (this.props.menu === EMenu.Options) {
            return (
                <div className="menu-content">
                    {this.props.menuContent}
                </div>
            );
        }
    }
}
