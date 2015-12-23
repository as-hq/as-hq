import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';


export default React.createClass({

  /*************************************************************************************************************************/
  // Sub-component generation

  fonts: [
    {name: 'Arial', family: 'Arial'},
    {name: 'Times New Roman', family: 'Times New Roman'},
  ],

  // Return a bunch of menu items, where each font is styled using its actual font, and an extra item for more fonts
  getMenuProps() {
    let menuItems = this.fonts.map((font) => {
      return {tag: 'MenuItem', primaryText: font.name, style: {fontFamily: font.family}, value: font.name};
    });
    let moreFontIcon = <FontIcon style={{backgroundColor: Styles.Colors.grey50}} className="material-icons"> {"add_box"} </FontIcon>;
    menuItems.push({
      tag: 'MenuItem', 
      leftFontIcon: moreFontIcon, 
      primaryText: "More Fonts", 
      value: "",
      onTouchTap: this._onMoreFonts});
    return menuItems;
  },

  toolbarControlProps() {
    return {
      displayValue: "Arial",
      tooltip: "Fonts",
      showTooltip: true,
      width: 200
    };
  },

  _onMoreFonts(e, index) {
    console.log("MORE FONTS CALLBACK");
  },

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell) {
    return "Arial"; // TODO: something like cell.cellProps.font.fontName
  },

  _propagateControlStateChange(nextValue, rng) {
    // TODO: case on value, call some API function
    console.log("Propagating state change to backend: " + nextValue);
  },

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  _toolbarControlPropTransform(menuVisible, menuValue, toolbarControlProps) {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.displayValue = menuValue;
    return toolbarControlProps;
  },

  /*************************************************************************************************************************/
  //Render

  render() {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.getMenuProps()}
        getMenuValueFromCell={this._getMenuValueFromCell}
        toolbarControlPropTransform={this._toolbarControlPropTransform}
        propagateControlStateChange={this._propagateControlStateChange}
        initialValue="Arial"
        menuWidth={65} 
        toolbarControlWidth={200}
        id="FontPicker" />
    );
  }

});


