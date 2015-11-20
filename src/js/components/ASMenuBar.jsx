import React from 'react';
import MenuBar from './menu/MenuBar.jsx'
import MenuItem from './menu/MenuItem.jsx'
import Menu from './menu/Menu.jsx'
import Separator from './menu/Separator.jsx'

import API from '../actions/ASApiActionCreators.js'

/*
  This component defines the top menu bar (File, Edit etc)
  It has an onSelect method that allows callbacks for user clicks on different menu items
*/

export default React.createClass({
  render: function () {
    return (
      <MenuBar onSelect={this.onSelect}>
        <MenuItem label="File">
          <Menu>
            <MenuItem command="clear"> Clear </MenuItem>
          </Menu>
        </MenuItem>

        <MenuItem label="Edit">
          <Menu>
            <MenuItem command="undo">Undo</MenuItem>
            <MenuItem command="redo">Redo</MenuItem>
            <Separator />
            <MenuItem label="Find">
              <Menu>
                <MenuItem command="find">Find…</MenuItem>
                <MenuItem command="find-next">Find Next</MenuItem>
                <MenuItem command="find-previous">Find Previous</MenuItem>
                <MenuItem command="use-selection-for-find">Use Selection For Find</MenuItem>
              </Menu>
            </MenuItem>
          </Menu>
        </MenuItem>

        <MenuItem label="Help">
          <Menu>
            <MenuItem command="Documentation">Documentation</MenuItem>
          </Menu>
        </MenuItem>
      </MenuBar>
    );
  },

  onSelect: function (command) {
    // console.log('Selected: %s', command);
    switch (command) {
      case "undo":
        API.sendUndoRequest();
        break;
      case "redo":
        API.sendRedoRequest();
        break;
      case "clear":
        API.sendClearRequest();
        break;
    }
  }

});
