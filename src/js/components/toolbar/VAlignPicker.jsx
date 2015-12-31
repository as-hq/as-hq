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
  ASCell
} from '../../types/Eval';

type VAlignPickerDefaultProps = {};

type VAlignPickerProps = {};

type VAlignPickerState = {};

export default class VAlignPicker
  extends React.Component<VAlignPickerDefaultProps, VAlignPickerProps, VAlignPickerState>
{
  constructor(props: VAlignPickerProps) {
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
      {tag: 'MenuItem', primaryText: 'Top', value: 'top', rightIcon: genIcon('vertical_align_top')},
      {tag: 'MenuItem', primaryText: 'Middle', value: 'center', rightIcon: genIcon('vertical_align_center')},
      {tag: 'MenuItem', primaryText: 'Bottom', value: 'bottom', rightIcon: genIcon('vertical_align_bottom')},
    ];
    return menuItems;
  }

  toolbarControlProps(): ToolbarControlProps {
    return {
      iconName: 'vertical_align_bottom',
      tooltip: 'Vertical align',
      includeDropdownArrow: true,
      usePushState: false,
      showTooltip: true,
      spacing: 7,
      width: 41
    };
  }

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell: ASCell): string {
    console.log("\n\n\nVAlign picker cell ", cell)
    return 'bottom'; // TODO: eventually cell.cellProps.valign
  }

  _propagateControlStateChange(nextValue: string, rng: NakedRange) {
    console.log("Propagating language change: " + nextValue);
    return; // TODO: eventually some API call
  }

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  // In this case, we want the icon to reflect the menu choice.
  _toolbarControlPropTransform(menuVisible: boolean, menuValue: string, toolbarControlProps: ToolbarControlProps): ToolbarControlProps {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.iconName = 'vertical_align_' + menuValue;
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
        initialValue={'bottom'}
        menuWidth={65} 
        id="VAlignPicker" />
    );
  }
}