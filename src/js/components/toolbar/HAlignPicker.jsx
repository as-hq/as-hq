/* @flow */

import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

import type {
  ToolbarControlProps, 
  MenuProps
} from '../../types/Toolbar';

import type {
  NakedRange,
  ASCellObject
} from '../../types/Eval';

type HAlignPickerDefaultProps = {};

type HAlignPickerProps = {};

type HAlignPickerState = {};

export default class HAlignPicker
  extends React.Component<HAlignPickerDefaultProps, HAlignPickerProps, HAlignPickerState>
{

  constructor(props: HAlignPickerProps) {
    super(props);
  }

  /*************************************************************************************************************************/
  // Sub-component generation

  // Return a bunch of menu items
  getMenuProps(): Array<MenuProps> {
    let genIcon = (iconName) => {
      return <FontIcon 
        style={{backgroundColor: Styles.Colors.grey400, color: Styles.Colors.grey800}} 
        className="material-icons"> 
          {iconName} 
        </FontIcon>;
    };
    let menuItems = [
      {tag: 'MenuItem', primaryText: 'Left', value: 'left', rightIcon: genIcon('format_align_left')},
      {tag: 'MenuItem', primaryText: 'Center', value: 'center', rightIcon: genIcon('format_align_center')},
      {tag: 'MenuItem', primaryText: 'Right', value: 'right', rightIcon: genIcon('format_align_right')},
    ];
    return menuItems;
  }

  toolbarControlProps(): ToolbarControlProps {
    return {
      iconName: 'format_align_left',
      tooltip: 'Horizontal align',
      usePushState: false,
      includeDropdownArrow: true,
      showTooltip: true,
      spacing: 7,
      width: 41
    };
  }

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell: ASCellObject): string {
    return 'left'; // TODO: eventually cell.cellProps.halign
  }

  _propagateControlStateChange(nextValue: string, rng: NakedRange) {
    console.log("Propagating language change: " + nextValue);
    return; // TODO: eventually some API call
  }

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  // In this case, we want the icon to reflect the menu choice.
  _toolbarControlPropTransform(menuVisible: boolean, menuValue: string, toolbarControlProps: ToolbarControlProps): ToolbarControlProps {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.iconName = 'format_align_' + menuValue;
    return toolbarControlProps;
  }

  /*************************************************************************************************************************/
  //Render

  render(): React.Element {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.getMenuProps()}
        getMenuValueFromCell={this._getMenuValueFromCell.bind(this)}
        toolbarControlPropTransform={this._toolbarControlPropTransform.bind(this)}
        propagateControlStateChange={this._propagateControlStateChange.bind(this)}
        initialValue={'left'}
        menuWidth={65} 
        id="HAlignPicker" />
    );
  }
}