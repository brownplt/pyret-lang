import React from 'react';

type Tab = {
    name: string,
    content: any[],
}

type MenuProps = {
    tabs: Tab[]
};

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
        return (
            <div className="menu-container">
                <div className="menu-tabbar">
                    {this.props.tabs.map((tab: Tab, index: number) => {
                        return (
                            <div className={(
                                this.state.visible && this.state.tab === index) ? (
                                    "menu-tab-active"
                                ) : (
                                    "menu-tab-inactive"
                                )}
                                 key={index}
                                 onClick={() => this.toggleTab(index)}>
                                {tab.name}
                            </div>
                        );
                    })}
                </div>
                {this.state.visible &&
                 <div className="menu-content">
                     {this.props.tabs[this.state.tab].content}
                 </div>}
            </div>
        );
    }
}
