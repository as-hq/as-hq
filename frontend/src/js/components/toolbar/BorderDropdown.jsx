/* @flow */

// #needsrefactor code/logic duplication with MoreFormatDropdown

import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';

import {FontIcon} from 'material-ui';

import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';
import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import type {
  ToolbarControlProps,
  MenuProps,
} from '../../types/Toolbar';

type BorderDropdownProps = {
  visible: boolean;
}

function fonticon(name: string): React.Element {
  return (
    <FontIcon className="material-icons"
              style={{margin: 0}}
              >
      {name}
    </FontIcon>
  );
}

// Give all props a secondaryText so that they'll be aligned in the same way in GenerateToolbarMenu
const menuProps: Array<MenuProps> = [
  {tag: 'MenuItem', primaryText: 'Outer', value: 'Outer', secondaryText: '', rightIcon: fonticon('border_outer')},
  {tag: 'MenuItem', primaryText: 'Inner', value: 'Inner', secondaryText: '', rightIcon: fonticon('border_inner')},
  {tag: 'MenuItem', primaryText: 'Top', value: 'Top', secondaryText: '', rightIcon: fonticon('border_top')},
  {tag: 'MenuItem', primaryText: 'Bottom', value: 'Bottom', secondaryText: '', rightIcon: fonticon('border_bottom')},
  {tag: 'MenuItem', primaryText: 'Left', value: 'Left', secondaryText: '', rightIcon: fonticon('border_left')},
  {tag: 'MenuItem', primaryText: 'Right', value: 'Right', secondaryText: '', rightIcon: fonticon('border_right')},
  {tag: 'MenuItem', primaryText: 'None', value: 'None', secondaryText: '', rightIcon: fonticon('border_clear')},
];

const toolbarControlProps: ToolbarControlProps = {
  tooltip: 'Set cell borders',
  iconName: 'border_style',
  showTooltip: true,
  includeDropdownArrow: true,
  width: 55,
};

const ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);

export default class BorderDropdown extends React.Component {
  static defaultProps = {};
  props: BorderDropdownProps;
  state: {};

  // TODO(joel) - make functional component
  shouldComponentUpdate(
    nextProps: BorderDropdownProps,
    nextState: any
  ): boolean {
    return shallowCompare(this, nextProps, nextState);
  }

  render(): React.Element {
    return (
      <ButtonWithMenu
        menuShouldCheckSelections={false}
        toolbarControlProps={toolbarControlProps}
        menuProps={menuProps}
        value="Number"
        menuWidth={150}
        visible={this.props.visible}
        onSelect={value => ToolbarActionCreators.setBorder(value)}
        onOpen={() => ToolbarActionCreators.openItem('Border')}
        onClose={() => ToolbarActionCreators.closeItem('Border')}
      />
    );
  }
}
