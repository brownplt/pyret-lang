import React from 'react';

export type DropdownOptionProps = {
    enabled: boolean,
    onClick: () => void,
};

export type DropdownOptionState = {};

export class DropdownOption extends React.Component<DropdownOptionProps, DropdownOptionState> {
    render() {
        const internalName = `DropdownOption${new Date().getTime()}`;

        return (
            <div className={this.props.enabled ? "run-option-enabled" : "run-option-disabled"}
                 onClick={this.props.onClick}>
                <input type="checkBox"
                       checked={this.props.enabled}
                       name={internalName}
                       className="run-option-checkbox"
                       readOnly={true}>
                </input>
                <label htmlFor={internalName}
                       className="run-option-label">
                    {this.props.children}
                </label>
            </div>
        );
    }
}

export type DropdownProps = {};

export type DropdownState = {};

export class Dropdown extends React.Component<DropdownProps, DropdownState> {
    render() {
        return (
            <div className="run-dropdown"
                 onClick={(e) => e.stopPropagation()}>
                {this.props.children}
            </div>
        );
    }
}
