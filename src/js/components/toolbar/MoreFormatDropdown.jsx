import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';


export default React.createClass({

  menuProps: [
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
  ],

  toolbarControlProps() {
    return {
      tooltip: "More formats",
      iconName: "wb_sunny",
      usePushState: false,
      showTooltip: true,
      includeDropdownArrow: true,
      width: 55
    };
  },

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell) {
    return "Number"; 
  },

  _propagateControlStateChange(nextValue, rng) {
    // TODO: case on value, call some API function
    console.log("Propagating state change to backend");
  },

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  _toolbarControlPropTransform(menuVisible, menuValue, toolbarControlProps) {
    toolbarControlProps.showTooltip = !menuVisible;
    return toolbarControlProps;
  },

  /*************************************************************************************************************************/
  //Render

  render() {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.menuProps}
        getMenuValueFromCell={this._getMenuValueFromCell}
        toolbarControlPropTransform={this._toolbarControlPropTransform}
        propagateControlStateChange={this._propagateControlStateChange}
        initialValue="Number"
        menuWidth={320}
        id="MoreFormatDropdown" />
    );
  }

});
