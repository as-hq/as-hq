import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarButton from './ToolbarButton.jsx';
import DropDownMenu from './DropDownMenu.jsx';


export default React.createClass({

  propTypes: {
    tooltipId: React.PropTypes.string.isRequired,
  },

  getDefaultProps() {
    return {
      
    };
  },

  /* We keep track of menu visiblility (needed for tooltip visibility) and the propTag used */
  getInitialState() {
    return {
      menuVisible: false,
      propTag: 'Money',
    }
  },

  /*************************************************************************************************************************/
  // Sub-component generation

  menuProps: [
    {tag: 'MenuItem', primaryText: 'Number', value: 'Number', secondaryText: '1000.12'},
    {tag: 'MenuItem', primaryText: 'Percent', value: 'Percent', secondaryText: '10.12%'},
    {tag: 'Divider', style:{marginTop: 5, marginBottom: 5}}
  ],

  getButton() {
    return (
      <ToolbarButton 
        ref="button"
        tooltipId={this.props.tooltipId}
        tooltip={"More formats"}
        iconName={"plus_one"}
        onClick={this._onButtonClick}
        showTooltip={!this.state.menuVisible} />
    );
  },

  /*************************************************************************************************************************/
  // Helper methods

  // This component deals with many propTags at once. Which one the toolbarController applies is dictated by the menu's
  // selected item. This converts from that value to the appropriate propTag. "Value" refers to the uid's used by the menu.
  getPropTagFromValue(v) {
    return v;
  },

  getValueFromProp(p) {
    return p.propTag;
  },

  // How should the button itself (icon, etc) change for this component if the value of the menu does 
  updateButtonGivenValue(v) {
    return;
  },

  /*************************************************************************************************************************/
  // Respond to events

  /* When the button is clicked, make the menu visible if it already isn't and vice versa. This will automatically
  make the tooltip disappear */
  _onButtonClick(e) {
    console.log("menu is visible");
    this.setState({ menuVisible: !this.state.menuVisible});
    this.refs.menu.toggleMenuVisible();
  },

  /* 
    Prop passed to DropDownMenu
    When a menu item is clicked, update the button and controller, then call a state change callback to update backend 
  */
  _onMenuClick(nextValue) {
    this.updateButtonGivenValue(nextValue);
    let tag = this.getPropTagFromValue(nextValue);
    this.setState({propTag: tag}, this.refs.controller.onControlStateChange(nextValue));
  },

  /* 
    Prop passed down to DropDownMenu
  */
  _onMenuClose() {
    this.setState({menuVisible: false});
    this.refs.button.setPushState(false);
  },

  /*************************************************************************************************************************/
  // ToolbarController callbacks

  /* Update backend given the nextState (the updated value of the menu) and the active range */
  _setBackendCellProp(nextState, rng) {
    console.log("Setting backend via api");
    return;
  },


  /* When the activeCell changes, update the menu with a new value, and update the button as well */
  _setControlStateFromCellProp(prop) {
    let menuValue = this.getValueFromProp(prop);
    this.refs.menu.setMenuValue(menuValue);
    this.updateButtonGivenValue(menuValue);
  },

  /*************************************************************************************************************************/
  //Render


  render() {
    let button = this.getButton(),
        menuProps = this.menuProps;
    let dropdown = 
      <DropDownMenu 
        ref="menu"
        menuProps={menuProps} 
        toolbarControl={button} 
        initialCheckedValue="Number"
        onMenuClick={this._onMenuClick}
        onMenuClose={this._onMenuClose} />;
    return (
      <ToolbarController 
        ref="controller"
        propTag={this.state.propTag}
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        setBackendCellProp={this._setBackendCellProp}
        control={dropdown} />
    );
  }

});
