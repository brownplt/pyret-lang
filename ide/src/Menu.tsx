// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { MenuItems } from './menu';
import { State, EditorMode } from './state';
import { Action } from './action';
import FSBrowser from './FSBrowser';
import FontSize from './FontSize';

type StateProps = {
  menuItems: MenuItems,
  menuTabVisible: false | number,
};

function mapStateToProps(state: State): StateProps {
  const { menuItems, menuTabVisible } = state;
  return { menuItems, menuTabVisible };
}

type DispatchProps = {
  setEditorMode: (mode: EditorMode) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    setEditorMode: (mode: EditorMode) => {
      dispatch({ type: 'update', key: 'editorMode', value: mode });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type MenuProps = PropsFromRedux & DispatchProps & StateProps;

function Menu({ menuItems, menuTabVisible, setEditorMode }: MenuProps) {
  function getTab() {
    if (menuTabVisible === false) {
      return false;
    }

    switch (menuItems[menuTabVisible].name) {
      case 'Files':
        return (
          <FSBrowser />
        );
      case 'Options':
        return (
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
            }}
          >
            <div
              style={{
                display: 'flex',
                height: '2.7em',
              }}
            >
              <button
                onClick={() => setEditorMode(EditorMode.Text)}
                className="option"
                key="TextEditor"
                type="button"
                style={{
                  width: '50%',
                }}
              >
                Text
              </button>
              <button
                onClick={() => setEditorMode(EditorMode.Chunks)}
                className="option"
                key="ChunkEditor"
                type="button"
                style={{
                  width: '50%',
                }}
              >
                Chunks
              </button>
            </div>
            <FontSize key="FontSize" />
          </div>
        );
      default:
        throw new Error(`Menu: unknown menu item name, ${menuItems[menuTabVisible].name}`);
    }
  }

  const tab = getTab();

  return (
    <div
      style={{
        height: '100%',
        background: '#c8c8c8',
        overflowY: tab === false ? undefined : 'scroll',
        minWidth: tab === false ? undefined : '16em',
      }}
    >
      {tab}
    </div>
  );
}

export default connector(Menu);

// export default class Menu extends React.Component<MenuProps, MenuState> {
//   constructor(props: MenuProps) {
//     super(props);
//
//     this.state = {
//       visible: false,
//       tab: 0,
//     };
//   }
//
//   toggleTab = (n: number): void => {
//     const { tab, visible } = this.state;
//     if (tab === n) {
//       this.setState({
//         visible: !visible,
//       });
//     } else {
//       this.setState({
//         tab: n,
//         visible: true,
//       });
//     }
//   };
//
//   render() {
//     const { children } = this.props;
//     const { visible, tab } = this.state;
//
//     function getChildArray() {
//       if (Array.isArray(children)) {
//         return children;
//       }
//       return [children];
//     }
//
//     const childArray = getChildArray();
//
//     const childNodes = childArray.map((childTab: any, index: number) => (
//       <div
//         className={(
//           visible && tab === index) ? (
//             'menu-tab-active'
//           ) : (
//             'menu-tab-inactive'
//           )}
//         key={childTab.props.name}
//         onClick={() => this.toggleTab(index)}
//       >
//         {childTab.props.icon}
//       </div>
//     ));
//
//     const content = childArray[tab];
//
//     return (
//       <div className="menu-container">
//         <div className="menu-tabbar">
//           {childNodes}
//         </div>
//         {visible && content}
//       </div>
//     );
//   }
// }
