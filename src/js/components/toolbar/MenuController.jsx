import React from 'react';
import Menu from 'material-ui/lib/menus/menu';
import MenuItem from 'material-ui/lib/menus/menu-item';
import Divider from 'material-ui/lib/divider';

/*
This is a higher order component which renders the toolbarMenu and the toolbarComponent
beside one another, and can handle checks next to selected items in the menu. 
*/

export default React.createClass({

  mixins: [
    require('react-clickaway')
  ],

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
    toolbarControlWidth: React.PropTypes.number.isRequired,
    toolbarControlHeight: React.PropTypes.number,
    menuWidth: React.PropTypes.number, 
    menuMaxHeight: React.PropTypes.number,
    onMenuClick: React.PropTypes.func,
    onMenuClose: React.PropTypes.func
  },

  getDefaultProps() {
    return {
      toolbarControlHeight: 36,
      shouldCheckSelection: true,
      initialCheckedValue: '',
      width: 36,
      height: 36,
      menuWidth: 200,
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
    console.log("clicked on item with value " + nextValue);
    if (this.props.shouldCheckSelection) {
      this.setState({checkedValue: nextValue});
    }
    this.props.onMenuClick(nextValue);
  },

  // Menu not visible on esc key
  _onMenuRequestClose(e) {
    this.setState({menuVisible: false});
    this.props.onMenuClose();
  },

  /*************************************************************************************************************************/
  // Menu generation

  // The tag is used to generate the type of element, and the props are used to generate a MenuItem/Divider etc. 
  // Automatically handles checking. Example of propSet: {tag: 'MenuItem', primaryText: 'Number' ... }
  getMenu() {
    let checkedValue = this.state.checkedValue;
    let menuItems = this.props.menuProps.map((p) => {
      let propSet = p;
      // If you should have checks, the value matches the checked value in state, and there's no default left icon, render check
      if (this.props.shouldCheckSelection && propSet.value === checkedValue && propSet.leftIcon == null) {
        propSet.checked = true;
        propSet.insetChildren = false;
      } else if (this.props.shouldCheckSelection) {
        propSet.insetChildren = true;
        propSet.checked = false;
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
        onEscKeyDown={this._onMenuRequestClose}>
          {menuItems}
      </Menu>
    );
  },

  /*************************************************************************************************************************/
  // Click away

  // Don't show menu when user clicks away
  componentClickAway() {
    this.setState({menuVisible: false});
  },

  /*************************************************************************************************************************/
  // Styling and rendering

  getStyles() {
    return {
      bottomControlStyle: {
        position: 'absolute', 
        display: 'inline-block', // this sets it just to the right of the button
        marginLeft:-this.props.toolbarControlWidth, 
        // ^ this moves it back to the left edge of the toolbar control; counter-act inline-block
        top: '50%',
        height: this.props.toolbarControlHeight,
        transform: 'translateY(50%)',
        // ^ Note that the toolbar control's bottom is at toolbarHeight/2 + controlHeight/2. The above transform puts
        // the top of the menu at this location. 
        zIndex: 50 // needed to display over the editor/sheet
      },
    };
  },

  render() {
    let {bottomControlStyle} = this.getStyles(),
        menuElement = this.getMenu();
    let menu = this.state.menuVisible ? menuElement : null;
    return (
      <span>
        {this.props.toolbarControl}
        <div style={bottomControlStyle}>
          {menu}
        </div>
      </span>
    );
  },

});

