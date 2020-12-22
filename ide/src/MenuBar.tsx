/* Component that can open / close menus. It is located inside the header at the
   top left of the screen with two buttons: "Files" and "Options" which open
   their corresponding menus.

   Were we not to be using Redux this component would be much harder to write;
   its logical children (the open or closed menus) are not its actual children
   in the DOM.

   The names (files and options) are initialized in state.ts. There's no good
   reason for this; it could be done here instead, and probably should be. */

// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Action } from './action';
import { MenuItems } from './menu-types';
import { State } from './state';

type StateProps = {
  menuItems: MenuItems,
  menuTabVisible: false | number,
};

function mapStateToProps(state: State): StateProps {
  const { menuItems, menuTabVisible } = state;
  return { menuItems, menuTabVisible };
}

type DispatchProps = {
  toggleTab: (tab: number) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    toggleTab(tab) {
      dispatch({ type: 'update', key: 'menuTabVisible', value: tab });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type MenuBarProps = PropsFromRedux & DispatchProps & StateProps;

function MenuBar({ menuItems, menuTabVisible, toggleTab }: MenuBarProps) {
  return (
    <div
      style={{
        display: 'flex',
        height: '100%',
      }}
    >
      {menuItems.map((item, index) => {
        const { name, id } = item;
        return (
          <button
            id={id}
            type="button"
            onClick={() => toggleTab(index)}
            style={{
              background: menuTabVisible === index ? 'darkgray' : '#979797',
              height: '100%',
              border: 'none',
              color: 'rgb(255, 255, 255)',
              marginRight: '0.1em',
            }}
            key={name}
          >
            {name}
          </button>
        );
      })}
    </div>
  );
}

export default connector(MenuBar);
