/* @flow */

import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ASCell from '../../classes/ASCell';
import ASRange from '../../classes/ASRange';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

import type {
  ToolbarControlProps,
  MenuProps
} from '../../types/Toolbar';

type FontSizePickerDefaultProps = {};

type FontSizePickerProps = {};

type FontSizePickerState = {};

export default class FontSizePicker
  extends React.Component<FontSizePickerDefaultProps, FontSizePickerProps, FontSizePickerState>
{

  /*************************************************************************************************************************/
  // Sub-component generation

  fontSizes: Array<number>;

  constructor(props: FontSizePickerProps) {
    super(props);

    this.fontSizes = [6, 7, 8, 9, 10, 11, 12, 14, 18, 24, 36];
  }


  getMenuProps(): Array<MenuProps> {
    return this.fontSizes.map((size) => {
      return {tag: 'MenuItem', primaryText: `${size}`, value: `${size}`};
    });
  }

  toolbarControlProps(): ToolbarControlProps {
    return {
      tooltip: "Font size",
      displayValue: '10',
      showTooltip: true,
      width: 45
    };
  }

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked
  _getMenuValueFromCell(cell: ASCell): string {
    return "10"; // TODO: something like cell.cellProps.font.fontSize
  }

  _propagateControlStateChange(nextValue: string, rng: ASRange) {
    // TODO: case on value, call some API function
    console.log("Propagating state change to backend: " + nextValue);
  }

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  _toolbarControlPropTransform(menuVisible: boolean, menuValue: string, toolbarControlProps: ToolbarControlProps): ToolbarControlProps {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.displayValue = menuValue;
    return toolbarControlProps;
  }

  /*************************************************************************************************************************/
  //Render

  render(): React.Element {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.getMenuProps()}
        getMenuValueFromCell={this._getMenuValueFromCell.bind(this)}
        toolbarControlPropTransform={this._toolbarControlPropTransform.bind(this)}
        propagateControlStateChange={this._propagateControlStateChange.bind(this)}
        initialValue="10"
        menuWidth={65}
        toolbarControlWidth={45}
        id="FontSizePicker" />
    );
  }

}
