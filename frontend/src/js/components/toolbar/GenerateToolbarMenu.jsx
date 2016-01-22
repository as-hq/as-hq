import React from 'react';
import Menu from 'material-ui/lib/menus/menu';
import MenuItem from 'material-ui/lib/menus/menu-item';
import Divider from 'material-ui/lib/divider';

import DropdownMenu from './DropdownMenu.jsx';

function GenerateToolbarMenu(ToolbarComponent) {
  return React.createClass({
    propTypes: {
      // Props to pass the toolbar control (ex width, height of button).
      toolbarControlProps: React.PropTypes.object.isRequired,
      // Props to generate menu
      menuProps: React.PropTypes.array.isRequired,
      // Width: menu width
      menuWidth: React.PropTypes.number,
      // Max height, will scroll if height > max height
      menuMaxHeight: React.PropTypes.number,
      // Should we check selections TODO(joel) - better description
      menuShouldCheckSelections: React.PropTypes.bool,
      toolbarControlWidth: React.PropTypes.number,
      visible: React.PropTypes.bool.isRequired,
      value: React.PropTypes.string.isRequired,
      onSelect: React.PropTypes.func.isRequired,
      onClose: React.PropTypes.func.isRequired,
      onOpen: React.PropTypes.func.isRequired,
    },

    // Easy way to deal with closing menu upon clickaway
    mixins: [require('react-clickaway')],

    getDefaultProps() {
      return {
        menuWidth: 65,
        menuMaxHeight: 400,
        menuShouldCheckSelections: true,
        toolbarControlWidth: 100,
      };
    },

    // The tag is used to generate the type of element, and the props are used
    // to generate a MenuItem/Divider etc.  Automatically handles checking.
    //
    // Example of propSet: {tag: 'MenuItem', primaryText: 'Number' ... }
    getMenu() {
      const checkedValue = this.props.value;
      const menuItems = this.props.menuProps.map((propSet) => {
        const valueMatch = propSet.value === checkedValue;
        const extraProps = {key: propSet.value};

        // If you should have checks, the value matches the checked value in
        // state, and there's no default left icon, render check
        if (!this.props.menuShouldCheckSelections) {
          // intentionally empty
        } else if (propSet.leftIcon == null) {
          extraProps.checked = valueMatch;
          extraProps.insetChildren = !valueMatch;
        } else {
          extraProps.checked = false;
          // if there's a leftIcon, inset children and never check
          extraProps.insetChildren = true;
        }

        return propSet.tag === 'MenuItem'
          ? <MenuItem {...propSet} {...extraProps} />
          : <Divider {...propSet} {...extraProps} />;
      });

      return (
        <Menu
          desktop
          width={this.props.menuWidth}
          maxHeight={this.props.menuMaxHeight}
          onItemTouchTap={this._onMenuClick}
          onEscKeyDown={this._onMenuClose}
        >
          {menuItems}
        </Menu>
      );
    },

    _onMenuClick(e, item) {
      this.props.onSelect(item.props.value);
    },

    componentClickAway() {
      if (this.props.visible) {
        this.props.onClose();
      }
    },

    _onToolbarControlClick() {
      this.props.visible ? this.props.onClose() : this.props.onOpen();
    },

    // Generate a menu element using menuProps, then generate a
    // toolbarComponent by using the given class along with
    // this.props.toolbarControlProps. Put them together with some styling, and
    // pass it off to ToolbarController, which monitors listening to the
    // relevant stores for us.
    render() {
      const menuElement = this.props.visible && this.getMenu();
      const toolbarComponent = (
        <ToolbarComponent
          {...this.props.toolbarControlProps}
          onClick={this._onToolbarControlClick}
        />
      );

      return (
        <DropdownMenu
          toolbarComponent={toolbarComponent}
          menuComponent={menuElement}
          toolbarWidth={this.props.toolbarControlWidth}
        />
      );
    },
  });
}

export default GenerateToolbarMenu;
