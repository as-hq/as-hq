import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import MenuController from './MenuController.jsx';

export default React.createClass({

   /*
  STATE: 
  this component is buggy when used when menucontroller. setting initialDisplayValue in fontpicker has no effect
  if the initialCheckedValue is different. Setting a large toolbar height causes menuitems to disappear for maybe css reasons of
  zIndex??
  */

  /* We keep track of menu visiblility (needed for tooltip visibility) and the propTag used */
  getInitialState() {
    return {
      menuVisible: false
    }
  },

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
    let moreFontIcon = <FontIcon style={{backgroundColor: Styles.Colors.green300}} className="material-icons"> {"add_box"} </FontIcon>;
    menuItems.push({tag: 'MenuItem', leftFontIcon: moreFontIcon, primaryText: "More Fonts", onItemTouchTap: this._onMoreFonts});
    return menuItems;
  },

  getFontField() {
    return (
      <ToolbarTextField 
        initialDisplayValue="Hello"
        ref="textfield"
        tooltip={"Fonts"}
        onClick={this._onTextFieldClick}
        showTooltip={!this.state.menuVisible}
        width={200} />
    );
  },

  /*************************************************************************************************************************/
  // Respond to events

  /* When the textfield is clicked, make the menu visible if it already isn't and vice versa. This will automatically
  make the tooltip disappear */
  _onTextFieldClick(e) {
    this.setState({ menuVisible: !this.state.menuVisible});
    this.refs.menu.toggleMenuVisible();
  },

  /* 
    Prop passed to MenuController
    When a menu item is clicked, update the button and controller, then call a state change callback to update backend 
  */
  _onMenuClick(nextValue) {
    console.log("font picker nextvalue: " + nextValue);
    this.refs.textfield.setDisplayValue(nextValue);
    this.refs.controller.onControlStateChange(nextValue);
  },

  /* Given a font prop, extract the value (font) from it */
  getValueFromProp(p) {
    return "Arial";
  },

  /* 
    Prop passed down to MenuController
  */
  _onMenuClose() {
    this.setState({menuVisible: false});
    this.refs.textfield.setActive(false);
  },

  // Callback when user wants to see more fonts
  _onMoreFonts(e, item) {
    console.log("more fonts");
  },

  /*************************************************************************************************************************/
  // ToolbarController callbacks

  /* Update backend given the nextState (the updated value of the menu) and the active range */
  _setBackendCellProp(nextState, rng) {
    console.log("Setting backend via api");
  },


  /* When the activeCell changes, update the menu with a new value, and update the button as well */
  _setControlStateFromCellProp(prop) {
    let menuValue = this.getValueFromProp(prop);
    this.refs.menu.setMenuValue(menuValue);
    this.refs.textfield.setDisplayValue(menuValue);
  },

  /*************************************************************************************************************************/
  //Render

  render() {
    let fontField = this.getFontField(),
        menuProps = this.getMenuProps();
    let dropdown = 
      <MenuController 
        ref="menu"
        toolbarControlWidth={200}
        menuProps={menuProps} 
        menuWidth={50}
        toolbarControl={fontField} 
        initialCheckedValue="Arial"
        onMenuClick={this._onMenuClick}
        onMenuClose={this._onMenuClose} />;
    return (
      <ToolbarController 
        ref="controller"
        propTag="Font"
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        setBackendCellProp={this._setBackendCellProp}
        control={dropdown} />
    );
  }

});
