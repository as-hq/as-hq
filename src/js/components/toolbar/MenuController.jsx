import React from 'react';
import Menu from 'material-ui/lib/menus/menu';
import MenuItem from 'material-ui/lib/menus/menu-item';
import ToolbarButton from 'material-ui/lib/divider';

import ToolbarController from './ToolbarController.jsx';
import ToolbarStore from '../../stores/ASToolbarStore';
import ToolbarActionCreator from '../../actions/ASToolbarActionCreators';

/*
This higher order component takes a toolbar component, a menu-style component, and styles them to be on top of one another.  
It also adds listeners to stores by invoking ToolbarController, and makes sure that at most one dropdown is open using the 
ToolbarStore.
*/

export default React.createClass({

  /*************************************************************************************************************************/
  // Prop and state methods

  /* 
    We need both components (menu and toolbar), 
    Callbacks to pass the ToolbarController, 
    Size data for styling,
    The uid of the menu component for uniquess of dropdown,
    A callback for when the menu needs to close due to uniquess of dropdown
  */
  propTypes: {
    toolbarComponent: React.PropTypes.object.isRequired,
    menuComponent: React.PropTypes.object.isRequired,
    setControlStateFromCell: React.PropTypes.func.isRequired,
    propagateControlStateChange: React.PropTypes.func.isRequired,
    toolbarWidth: React.PropTypes.number,
    toolbarHeight: React.PropTypes.number,
    id: React.PropTypes.string.isRequired,
    onMenuShouldClose: React.PropTypes.func.isRequired
  },

  getDefaultProps(){
    return {
      toolbarWidth: 100, 
      toolbarHeight: 36
    }
  },

  /*************************************************************************************************************************/
  // Mounting
  // We have a toolbar store to make sure that at most one dropdown is open at a time. 

  componentDidMount() {
    // After the initial render, inform the ToolbarStore of our existence
    ToolbarStore.addChangeListener(this._onDropdownClicked);
    this._informStore();
  },

  componentDidUpdate(prevProps, prevState) {
    // After a non-initial render, inform the ToolbarStore of our existence
    this._informStore();
  },

  componentWillUnmount() {
    // Inform the ToolbarStore that we're gone
    this._informStore();
    ToolbarStore.removeChangeListener(this._onDropdownClicked);
  },

  // Fire an action if the ToolbarStore has out-of-date info
  _informStore() {
    let lastClickedId = ToolbarStore.getLastClickedId();
    if (!this._menuVisible() && lastClickedId !== this.props.id) {
      // If I just closed but the lastClickedId isn't me anyway, no need to update
      // You only need to update the store when you close if the store thinks you're open
      return;
    } else if (this._menuVisible() && lastClickedId === this.props.id) {
      // If I just opened but was already open in the store's view, no need to update
      return;
    } else {
      ToolbarActionCreator.click(this._menuVisible(), this.props.id);
    }
  },

  _menuVisible() {
    return this.props.menuComponent != null;
  },

  // When the store has a change event, some dropdown was clicked. If it's not this dropdown, and this dropdown is visible, 
  // inform the parent, which will update its state and then render the menu invisible.
  _onDropdownClicked() {
    let lastClickedId = ToolbarStore.getLastClickedId();
    debugger;
    if (lastClickedId !== this.props.id && this._menuVisible()) {
      this.props.onMenuShouldClose();
    }
  },


  /*************************************************************************************************************************/
  // Styling and rendering

  getStyles() {
    let width = this.props.toolbarWidth;
    let height = this.props.toolbarHeight;
    return {
      // Styling element for the menu
      menuStyle: {
        position: 'absolute', 
        display: 'inline-block', // this sets it just to the right of the button
        marginLeft:-width, 
        // ^ this moves it back to the left edge of the toolbar control; counter-act inline-block
        top: '50%',
        height: height,
        transform: 'translateY(50%)',
        // ^ Note that the toolbar control's bottom is at toolbarHeight/2 + controlHeight/2. The above transform puts
        // the top of the menu at this location. 
        zIndex: 50 // needed to display over the editor/sheet
      },
    };
  },

  render() {
    let {menuStyle} = this.getStyles();
    let toolbarComponentWithMenu = 
      <span>
        {this.props.toolbarComponent}
        <div style={menuStyle}>
          {this.props.menuComponent}
        </div>
      </span>;
    return (
       <ToolbarController 
          ref="controller"
          setControlStateFromCell={this.props.setControlStateFromCell}
          propagateControlStateChange={this.props.propagateControlStateChange}
          control={toolbarComponentWithMenu}/>
    );
  }
});







