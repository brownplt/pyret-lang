import React from 'react';

type TabProps = {
    name: any,
};

type TabState = {};

export class Tab extends React.Component<TabProps, TabState> {
    render() {
        return <div className="menu-content">{this.props.children}</div>
    }
}

type MenuProps = {};

type MenuState = {
    visible: boolean,
    tab: number,
};

export class Menu extends React.Component<MenuProps, MenuState> {
    constructor(props: MenuProps) {
        super(props);

        this.state = {
            visible: false,
            tab: 0,
        };
    }

    toggleTab = (n: number): void => {
        if (this.state.tab === n) {
            this.setState({
                visible: !this.state.visible,
            });
        } else {
            this.setState({
                tab: n,
                visible: true,
            })
        }
    };

    render() {
        const childNodes = Array.isArray(this.props.children) &&
                           this.props.children.map((tab: any, index: number) => {
                               return (
                                   <div className={(
                                       this.state.visible && this.state.tab === index) ? (
                                           "menu-tab-active"
                                       ) : (
                                           "menu-tab-inactive"
                                       )}
                                        key={index}
                                        onClick={() => this.toggleTab(index)}>
                                       {tab.props.name}
                                   </div>
                               );
                           });
        const content = Array.isArray(this.props.children) &&
                        this.props.children[this.state.tab];
        return (
            <div className="menu-container">
                <div className="menu-tabbar">
                    {childNodes}
                </div>
                {this.state.visible && content}
            </div>
        );
    }
}
