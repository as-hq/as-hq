// @flow
// This higher order component takes a toolbar component and a menu-style
// component, and styles them to be on top of one another.

import React from 'react';

type DropdownMenuDefaultProps = {
  toolbarWidth: number;
  toolbarHeight: number;
};

type DropdownMenuProps = {
  toolbarComponent: React.Element;
  menuComponent: React.Element;
  toolbarWidth: number;
  toolbarHeight: number;
};

export default class DropdownMenu extends React.Component {
  static defaultProps: DropdownMenuDefaultProps = {
    toolbarWidth: 100,
    toolbarHeight: 36
  };

  props: DropdownMenuProps;
  state: {};


  // TODO(joel) - don't recompute this every render. Move styles out of
  // component.
  getStyles(): any {
    const {toolbarWidth, toolbarHeight} = this.props;
    return {
      // Styling element for the menu
      menuStyle: {
        position: 'absolute',
        display: 'inline-block', // this sets it just to the right of the button
        marginLeft: -toolbarWidth,
        // ^ this moves it back to the left edge of the toolbar control; counter-act inline-block
        top: '50%',
        height: toolbarHeight,
        transform: 'translateY(50%)',
        // ^ Note that the toolbar control's bottom is at toolbarHeight/2 + controlHeight/2. The above transform puts
        // the top of the menu at this location.
        zIndex: 50, // needed to display over the editor/sheet
      },
    };
  }

  render(): React.Element {
    const {menuStyle} = this.getStyles();
    const {toolbarComponent, menuComponent} = this.props;

    return (
      <div>
        {toolbarComponent}
        <div style={menuStyle}>
          {menuComponent}
        </div>
      </div>
    );
  }
}
