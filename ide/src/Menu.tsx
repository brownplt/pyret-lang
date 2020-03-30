// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';

type MenuProps = {};

type MenuState = {
  visible: boolean,
  tab: number,
};

export default class Menu extends React.Component<MenuProps, MenuState> {
  constructor(props: MenuProps) {
    super(props);

    this.state = {
      visible: true,
      tab: 0,
    };
  }

  toggleTab = (n: number): void => {
    const { tab, visible } = this.state;
    if (tab === n) {
      this.setState({
        visible: !visible,
      });
    } else {
      this.setState({
        tab: n,
        visible: true,
      });
    }
  };

  render() {
    const { children } = this.props;
    const { visible, tab } = this.state;

    function getChildArray() {
      if (Array.isArray(children)) {
        return children;
      }
      return [children];
    }

    const childArray = getChildArray();

    const childNodes = childArray.map((childTab: any, index: number) => (
      <div
        className={(
          visible && tab === index) ? (
            'menu-tab-active'
          ) : (
            'menu-tab-inactive'
          )}
        key={childTab.props.name}
        onClick={() => this.toggleTab(index)}
      >
        {childTab.props.name}
      </div>
    ));

    const content = childArray[tab];

    return (
      <div className="menu-container">
        <div className="menu-tabbar">
          {childNodes}
        </div>
        {visible && content}
      </div>
    );
  }
}
