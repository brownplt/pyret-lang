// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { MenuItems } from './menu-types';
import { State, EditorMode, EditorLayout, getCurrentFileContents } from './state';
import { Action } from './action';
import FSBrowser from './FSBrowser';
import FontSize from './FontSize';
import { CHUNKSEP } from './chunk';

type StateProps = {
  menuItems: MenuItems,
  menuTabVisible: false | number,
  enterNewline: boolean,
  editorMode: EditorMode,
  currentFileContents: string | undefined,
  editorLayout : EditorLayout,
  developerMode : boolean,
};

function mapStateToProps(state: State): StateProps {
  const {
    menuItems,
    menuTabVisible,
    enterNewline,
    editorMode,
    editorLayout,
    developerMode,
  } = state;

  return {
    menuItems,
    menuTabVisible,
    enterNewline,
    editorMode,
    editorLayout,
    developerMode,
    currentFileContents: getCurrentFileContents(state)
  };
}

type DispatchProps = {
  update: (updater: (s: State) => State) => void,
  setEditorMode: (mode: EditorMode) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    update: (value: (s: State) => State) => {
      dispatch({ type: 'update', key: 'updater', value });
    },
    setEditorMode: (mode: EditorMode) => {
      dispatch({ type: 'update', key: 'editorMode', value: mode });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type MenuProps = PropsFromRedux & DispatchProps & StateProps;

function Menu({
  menuItems,
  menuTabVisible,
  update,
  setEditorMode,
  enterNewline,
  editorMode,
  currentFileContents,
  editorLayout,
  developerMode,
}: MenuProps) {
  function getTab() {
    if (menuTabVisible === false) {
      return false;
    }

    const swapLayout = () => (
      editorLayout === EditorLayout.Compact ? EditorLayout.Normal : EditorLayout.Compact
    );

    const modes = [EditorMode.Chatitor, EditorMode.Text, EditorMode.Examplaritor];
    const modesElement = (
      <div
        style={{
          display: 'flex',
          height: '2.7em',
        }}
      >
        {modes.map((mode) => (
          <button
            onClick={() => setEditorMode(mode)}
            className="option"
            key={mode}
            type="button"
            style={{
              width: `${Math.floor(100 * modes.length)}%`,
            }}
          >
            {mode}
          </button>
        ))}
      </div>
    );

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
            <FontSize key="FontSize" />
            {editorMode === EditorMode.Chatitor && (
              <button
                onClick={() => {
                  if (currentFileContents !== undefined) {
                    const url = `${document.location.origin}${document.location.pathname}?program=${encodeURIComponent(currentFileContents)}`;
                    navigator.clipboard.writeText(url);
                  }
                }}
                className="option"
                key="getShareableLink"
                type="button"
                style={{
                  height: '2.7em',
                  width: '100%',
                }}
              >
                Get shareable link
              </button>
            )}
            <button
              className="option"
              onClick={() => {
                const noChunks = (currentFileContents || '').replaceAll(CHUNKSEP, '\n');
                navigator.clipboard.writeText(noChunks);
              }}
              type="button"
            >
              Copy as Program
            </button>
            {editorMode === EditorMode.Chatitor && developerMode && (
              <button
                onClick={() => update((s) => ({ ...s, editorLayout: swapLayout() }))}
                className="option"
                key="layout"
                type="button"
              >
                {editorLayout === EditorLayout.Compact ? (
                  'Switch to chat layout'
                ) : (
                  'Switch to compact layout'
                )}
              </button>
            )}

            {modesElement}

            {editorMode === EditorMode.Chatitor && (
              <button
                onClick={() => update((s) => ({ ...s, enterNewline: !s.enterNewline }))}
                className="option"
                key="enterNewline"
                type="button"
                style={{
                  height: '2.7em',
                }}
              >
                {enterNewline ? (
                  'Enter adds new line (click to change)'
                ) : (
                  'Enter can send chats (click to change)'
                )}
              </button>
            )}
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
