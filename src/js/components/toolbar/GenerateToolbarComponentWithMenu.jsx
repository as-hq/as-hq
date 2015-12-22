import React from 'react';
import Menu from 'material-ui/lib/menus/menu';
import MenuItem from 'material-ui/lib/menus/menu-item';
import Divider from 'material-ui/lib/divider';

import ToolbarController from './ToolbarController.jsx';
import MenuController from './MenuController.jsx';

let GenerateToolbarComponentWithMenu = function(ToolbarComponent) {

  return React.createClass({

      // Easy way to deal with closing menu upon clickaway
      mixins: [require('react-clickaway')],

      /*************************************************************************************************************************/
      // Prop and state methods

      /* 
        We use the following props:
          1) Props to pass the toolbar control (ex width, height of button). 
          2) Props to generate menu
          3) How to get the menu value given a change in active cell; function(cell) -> value
          4) toolbarControlPropTransform: function(menuVisible, menuValue, toolbarProps) -> newToolbarProps
          5) How to propagate a state change in the control, usually some API call
          6) Initial value in menu
          7) Width: menu width
          8) Max height, will scroll if height > max height
      */
      propTypes: {
        toolbarControlProps: React.PropTypes.object.isRequired,
        menuProps: React.PropTypes.array.isRequired,
        getMenuValueFromCell: React.PropTypes.func.isRequired,
        toolbarControlPropTransform: React.PropTypes.func.isRequired,
        propagateControlStateChange: React.PropTypes.func.isRequired,
        initialValue: React.PropTypes.string.isRequired,
        menuWidth: React.PropTypes.number,
        menuMaxHeight: React.PropTypes.number,
        menuShouldCheckSelections: React.PropTypes.bool,
        id: React.PropTypes.string.isRequired
      },

      getDefaultProps() {
        return {
          menuWidth: 65,
          menuMaxHeight: 400,
          menuShouldCheckSelections: true
        }
      },

      getInitialState() {
        return {
          toolbarControlProps: this.props.toolbarControlProps,
          menuVisible: false,
          menuValue: this.props.initialValue
        }
      },

      /*************************************************************************************************************************/
      // Menu generation

      // The tag is used to generate the type of element, and the props are used to generate a MenuItem/Divider etc. 
      // Automatically handles checking. Example of propSet: {tag: 'MenuItem', primaryText: 'Number' ... }
      getMenu() {
        let checkedValue = this.state.menuValue;
        let menuItems = this.props.menuProps.map((propSet) => {
          let valueMatch =  propSet.value === checkedValue;
          // If you should have checks, the value matches the checked value in state, and there's no default left icon, render check
          if (!this.props.menuShouldCheckSelections) {
          } else if (propSet.leftIcon == null) {
            propSet.checked = valueMatch;
            propSet.insetChildren = !valueMatch;
          } else {
            propSet.checked = false;
            propSet.insetChildren = true; // if there's a leftIcon, inset children and never check
          }
          if (propSet.tag === 'MenuItem') {
            return React.createElement(MenuItem, propSet)
          } else {
            return React.createElement(Divider, propSet)
          }
        });
        return (
          <Menu
            desktop={true} 
            width={this.props.menuWidth} 
            maxHeight={this.props.menuMaxHeight} 
            onItemTouchTap={this._onMenuClick}
            onEscKeyDown={this._onMenuClose}>
              {menuItems}
          </Menu>
        );
      },


      /*************************************************************************************************************************/
      // Respond to menu events

      // When an item is clicked, update checkedValue state and call the callback
      _onMenuClick(e, item) {
        let nextValue = item.props.value; 
        let {toolbarControlProps} = this.state;
        let newToolbarControlProps = this.props.toolbarControlPropTransform(false, nextValue, toolbarControlProps);
        this.setState({menuVisible: false, menuValue: nextValue, toolbarControlProps: newToolbarControlProps});
        console.log("clicked on item with value " + nextValue);
        this.refs.menuController.refs.controller.onControlStateChange(nextValue);
      },

      _onMenuClose() {
        let {toolbarControlProps, menuValue} = this.state;
        let newToolbarControlProps = this.props.toolbarControlPropTransform(false, menuValue, toolbarControlProps);
        this.setState({menuVisible: false, toolbarControlProps: newToolbarControlProps});
      },

      /*************************************************************************************************************************/
      // Click away

      // Close menu when user clicks away if menu is currently visible
      componentClickAway() {
        if (this.state.menuVisible){
          this._onMenuClose();
        }
      },

      /*************************************************************************************************************************/
      // Toolbar component onClick

      // When you click on the toolbar control, toggle menu visibility
      _onToolbarControlClick(e, nextToolbarState) {
        console.log("generator toolbar control onclick ");
        let menuVisible = !this.state.menuVisible;
        let {toolbarControlProps, menuValue} = this.state;
        let newToolbarControlProps = this.props.toolbarControlPropTransform(menuVisible, menuValue, toolbarControlProps);
        this.setState({menuVisible: menuVisible, toolbarControlProps: newToolbarControlProps});
      },

      /*************************************************************************************************************************/
      // ToolbarController callbacks

      /* When the activeCell changes, update the menu with a new value, and update the toolbar component as well */
      _setControlStateFromCell(cell) {
        console.log("Setting control state from cell in generator");
        let menuValue = this.props.getMenuValueFromCell(cell);
        let {toolbarControlProps, menuVisible} = this.state;
        let newToolbarControlProps = this.props.toolbarControlPropTransform(menuVisible, menuValue, toolbarControlProps);
        this.setState({menuValue: menuValue, toolbarControlProps: newToolbarControlProps});
      },

      /*************************************************************************************************************************/
      // Rendering

      // Generate a menu element using menuProps, then generate a toolbarComponent by using the given class along with 
      // this.props.toolbarControlProps. Put them together with some styling, and pass it off to ToolbarController, which 
      // monitors listening to the relevant stores for us. 
      render() {
        let menuElement = this.state.menuVisible ? this.getMenu() : null;
        let toolbarComponent = 
          <ToolbarComponent 
            {...this.state.toolbarControlProps} 
            onClick={this._onToolbarControlClick} 
            ref="toolbarControl"/>;
        return (
          <MenuController
            ref="menuController"            
            toolbarComponent={toolbarComponent}
            menuComponent={menuElement}
            propagateControlStateChange={this.props.propagateControlStateChange}
            setControlStateFromCell={this._setControlStateFromCell}
            id={this.props.id}
            onMenuShouldClose={this._onMenuClose}/>
        );
      }
  });
}

export default GenerateToolbarComponentWithMenu;






