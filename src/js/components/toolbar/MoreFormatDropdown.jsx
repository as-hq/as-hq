/* @flow */

import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

import ASCell from '../../classes/ASCell';

import type {
  ToolbarControlProps,
  MenuProps
} from '../../types/Toolbar';

import type {
  NakedRange
} from '../../types/Eval';

type MoreFormatDropdownDefaultProps = {};

type MoreFormatDropdownProps = {};

type MoreFormatDropdownState = {};

export default class MoreFormatDropdown
  extends React.Component<MoreFormatDropdownDefaultProps, MoreFormatDropdownProps, MoreFormatDropdownState>
{

  menuProps: Array<MenuProps>;

  constructor(props: MoreFormatDropdownProps) {
    super(props);

    this.menuProps = [
      {tag: 'MenuItem', primaryText: 'Automatic', value: 'Automatic'},
      {tag: 'MenuItem', primaryText: 'Plain Text', value: 'Plain Text'},
      {tag: 'Divider', style:{marginTop: 5, marginBottom: 5}},
      {tag: 'MenuItem', primaryText: 'Number', value: 'Number', secondaryText: '1000.12'},
      {tag: 'MenuItem', primaryText: 'Percent', value: 'Percent', secondaryText: '10.12%'},
      {tag: 'MenuItem', primaryText: 'Scientific', value: 'Scientific', secondaryText: '1.01E+03'},
      {tag: 'Divider', style:{marginTop: 5, marginBottom: 5}},
      {tag: 'MenuItem', primaryText: 'Financial', value: 'Financial', secondaryText: '(1000.12)'},
      {tag: 'MenuItem', primaryText: 'Currency', value: 'Currency', secondaryText: '$1000.12'},
      {tag: 'Divider', style:{marginTop: 5, marginBottom: 5}},
    ];
  }

  toolbarControlProps(): ToolbarControlProps {
    return {
      tooltip: "More formats",
      iconName: "wb_sunny",
      usePushState: false,
      showTooltip: true,
      includeDropdownArrow: true,
      width: 55
    };
  }

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked
  _getMenuValueFromCell(cell: ASCell): string {
    return "Number";
  }

  _propagateControlStateChange(nextValue: string, rng: NakedRange) {
    // TODO: case on value, call some API function
    console.log("Propagating state change to backend");
  }

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  _toolbarControlPropTransform(menuVisible: boolean, menuValue: string, toolbarControlProps: ToolbarControlProps): ToolbarControlProps {
    toolbarControlProps.showTooltip = !menuVisible;
    return toolbarControlProps;
  }

  /*************************************************************************************************************************/
  //Render

  render(): React.Element {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.menuProps}
        getMenuValueFromCell={this._getMenuValueFromCell.bind(this)}
        toolbarControlPropTransform={this._toolbarControlPropTransform.bind(this)}
        propagateControlStateChange={this._propagateControlStateChange.bind(this)}
        initialValue="Number"
        menuWidth={320}
        id="MoreFormatDropdown" />
    );
  }
}
