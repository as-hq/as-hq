import React from 'react';
import Menu from 'material-ui/lib/menus/menu';
import MenuItem from 'material-ui/lib/menus/menu-item';
import Divider from 'material-ui/lib/divider';

/*
This is a higher order component which renders the toolbarMenu and the toolbarComponent
beside one another, and can handle checks next to selected items in the menu. 
*/

export default React.createClass({

  /*************************************************************************************************************************/
  // Prop and state methods

  /* 
    We use the following props:
      1) The toolbar control element, assumed to be already styled for fitting in the toolbar
      2) menuProps: an array of objects, each object must have a tag and a uid value. Used to make MenuItems
      3) Is there checking of selections?
      4) Initial checked value
      5,6,7,8) size parameters. If menu height >= maxHeight, the menu scrolls. 
      9) Callback for menu clicking, after internally updating state. Takes in the selected value.  
      10) Callback when the menu closes
  */
  propTypes: {
    toolbarControl: React.PropTypes.object.isRequired,
    menuProps: React.PropTypes.array.isRequired,
    shouldCheckSelection: React.PropTypes.boolean,
    initialCheckedValue: React.PropTypes.string,
    toolbarControlWidth: React.PropTypes.number,
    toolbarControlHeight: React.PropTypes.number,
    menuWidth: React.PropTypes.number, 
    menuMaxHeight: React.PropTypes.number,
    onMenuClick: React.PropTypes.func,
    onMenuClose: React.PropTypes.func
  },

  getDefaultProps() {
    return {
      shouldCheckSelection: true,
      initialCheckedValue: '',
      width: 36,
      height: 36,
      menuWidth: 300,
      menuMaxHeight: 400,
      onMenuClick: (e, item) => {},
      onMenuClose: () => {}
    }
  },

  /* We keep track of the checked value, possibly null if no checking allowed, and if the menu is visible or not */
  getInitialState() {
    return {
      checkedValue: (this.props.shouldCheckSelection ? this.props.initialCheckedValue : null),
      menuVisible: false
    }
  },

  /*************************************************************************************************************************/
  // Respond to events

  // Setting the menu value due to external events, will modify location of check if applicable
  setMenuValue(value) {
    if (this.props.shouldCheckSelection) {
      this.setState({checkedValue: value});
    }
  },

  // Called by a parent who wants to toggle visibility
  toggleMenuVisible() {
    console.log("toggling menu visibility");
    this.setState({menuVisible: !this.state.menuVisible});
  },

  // When an item is clicked, update checkedValue state and call the callback
  _onMenuClick(e, item) {
    let nextValue = item.props.value; 
    if (this.props.shouldCheckSelection) {
      this.setState({checkedValue: nextValue});
    }
    this.props.onMenuClick(nextValue);
  },

  // Menu not visible on esc key
  _onEscKeyDown(e) {
    this.setState({menuVisible: false});
    this.props.onMenuClose();
  },

  /*************************************************************************************************************************/
  // Menu generation

  // The tag is used to generate the type of element, and the props are used to generate a MenuItem/Divider etc. 
  // Automatically handles checking. Example of propSet: {tag: 'MenuItem', primaryText: 'Number' ... }
  getMenu() {
    let checkedValue = this.state.checkedValue;
    let menuItems = this.props.menuProps.map((propSet) => {
      if (this.props.shouldCheckSelection && propSet.value === checkedValue) {
        propSet.checked = true;
        propSet.insetChildren = false;
      } else if (this.props.shouldCheckSelection) {
        propSet.insetChildren = true;
        propSet.checked = false;
      } 
      if (propSet.tag == 'MenuItem') {
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
        onEscKeyDown={this._onEscKeyDown}>
          {menuItems}
      </Menu>
    );
  },

  // <MenuItem primaryText="Number" value="Number" secondaryText="hi" />
  //       <MenuItem insetChildren={true} primaryText="Percent" value="Percent" secondaryText="hi" />

  /*************************************************************************************************************************/
  // Styling and rendering

  getStyles() {
    return {
      bottomControlStyle: {
        position: 'absolute', 
        display: 'inline-block', // this sets it just to the right of the button
        marginLeft:-this.props.width, // this moves it back to the left edge of the button
        top: this.props.height, // this moves it to right below the button, and depends on toolbarHeight for accuracy
        zIndex: 50 // needed to display over the editor/sheet
      },
      menuStyle: {

      }
    };
  },

  render() {
    let {bottomControlStyle} = this.getStyles(),
        menuElement = this.getMenu();
    let menu = this.state.menuVisible ? menuElement : null;
    return (
      <span>
        {this.props.toolbarControl}
        <div style={bottomControlStyle} >
          {menu}
        </div>
      </span>
    );
  },

});

