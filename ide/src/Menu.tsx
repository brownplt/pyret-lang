import React from 'react';
import * as control from './control';

export enum EMenu {
    FSBrowser,
    Options,
}

export type FSItemProps = {
    onClick: () => void;
    contents: string;
};

export type FSItemState = {};

export class FSItem extends React.Component<FSItemProps, FSItemState> {
    get contents() {
        return this.props.contents;
    }

    render() {
        return (
            <button onClick={this.props.onClick}
                    className="fs-browser-item">
                {this.props.contents}
            </button>
        );
    }
}

type MenuProps = {
    menu: EMenu,
    browsingRoot: boolean,
    onTraverseUp: (path: string[]) => void,
    onTraverseDown: (path: string[]) => void,
    onExpandChild: (child: string, fullChildPath: string) => void,
    browsePath: string[],
    decreaseFontSize: () => void,
    increaseFontSize: () => void,
    resetFontSize: () => void,
    fontSize: number,
};
type MenuState = {};

export class Menu extends React.Component<MenuProps, MenuState> {
    get browsePathString() {
        return control.bfsSetup.path.join(...this.props.browsePath);
    }

    static compareFSItemPair =
        (a: [string, FSItem],
         b: [string, FSItem]): any => {
        if (a[0] < b[0]) {
            return -1;
        } else if (a[0] > b[0]) {
            return 1;
        } else {
            return 0;
        }
    };

    traverseUp = (): void => {
        const newPath = this.props.browsePath.slice();
        newPath.pop();

        this.props.onTraverseUp(newPath);
    };

    traverseDown = (childDirectory: string): void => {
        const newPath = this.props.browsePath.slice();
        newPath.push(childDirectory);

        this.props.onTraverseDown(newPath);
    };

    expandChild = (child: string): void => {
        const fullChildPath =
            control.bfsSetup.path.join(this.browsePathString, child);
        const stats = control.fs.statSync(fullChildPath);

        if (stats.isDirectory()) {
            this.traverseDown(child);
        } else if (stats.isFile()) {
            this.props.onExpandChild(child, fullChildPath);
        }
    }

    createFSItemPair = (filePath: string): [string, any] => {
        return [
            filePath,
            <FSItem key={filePath}
                    onClick={() => this.expandChild(filePath)}
                    contents={filePath}/>
        ];
    };

    render() {
        if (this.props.menu === EMenu.FSBrowser) {
            return (
                <div className="menu-content">
                    {!this.props.browsingRoot && (
                        <button className="fs-browser-item"
                                onClick={this.traverseUp}>
                            ..
                        </button>
                    )}
                    {
                        control.fs
                               .readdirSync(this.browsePathString)
                               .map(this.createFSItemPair)
                               .sort(Menu.compareFSItemPair)
                               .map((x: [string, FSItem]) => x[1])
                    }
                </div>
            );
        } else if (this.props.menu === EMenu.Options) {
            return (
                <div className="menu-content">
                    <div className="font-size-options">
                        <button className="font-minus"
                                onClick={this.props.decreaseFontSize}>
                            -
                        </button>
                        <button className="font-label"
                                onClick={this.props.resetFontSize}>
                            Font ({this.props.fontSize} px)
                        </button>
                        <button className="font-plus"
                                onClick={this.props.increaseFontSize}>
                            +
                        </button>
                    </div>
                </div>
            );
        }
    }
}
