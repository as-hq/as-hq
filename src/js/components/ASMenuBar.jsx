import React from 'react';
import MenuBar from './menu/MenuBar.jsx'
import MenuItem from './menu/MenuItem.jsx'
import Menu from './menu/Menu.jsx'
import Separator from './menu/Separator.jsx'

import API from '../actions/ASApiActionCreators.js'
 
export default React.createClass({
  render: function () {
    return (
      <MenuBar onSelect={this.onSelect}>
        <MenuItem label="File">
          <Menu>
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
    console.log('Selected: %s', command);
    if (command === "undo"){
      API.sendUndoRequest();
    }
    if (command === "redo"){
      API.sendRedoRequest();
    }
  }

});