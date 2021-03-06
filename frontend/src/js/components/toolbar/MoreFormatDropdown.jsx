/* @flow */

import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';

import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';
import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import type {
  ToolbarControlProps,
  MenuProps,
} from '../../types/Toolbar';

type MoreFormatDropdownProps = {
  visible: boolean;
}

// Give all props a secondaryText so that they'll be aligned in the same way in GenerateToolbarMenu
const menuProps: Array<MenuProps> = [
  {tag: 'MenuItem', primaryText: 'Automatic', value: 'Automatic', secondaryText: '', disabled: true},
  {tag: 'MenuItem', primaryText: 'Plain Text', value: 'Plain Text', secondaryText: '', disabled: true},
  {tag: 'Divider', style: {marginTop: 5, marginBottom: 5}, disabled: true},
  {tag: 'MenuItem', primaryText: 'Number', value: 'Number', secondaryText: '1000.12', disabled: true},
  {tag: 'MenuItem', primaryText: 'Percent', value: 'Percent', secondaryText: '10.12%', disabled: true},
  {tag: 'MenuItem', primaryText: 'Scientific', value: 'Scientific', secondaryText: '1.01E+03', disabled: true},
  {tag: 'Divider', style: {marginTop: 5, marginBottom: 5}, disabled: true},
  {tag: 'MenuItem', primaryText: 'Financial', value: 'Financial', secondaryText: '(1000.12)', disabled: true},
  {tag: 'MenuItem', primaryText: 'Currency', value: 'Currency', secondaryText: '$1000.12', disabled: true},
  {tag: 'Divider', style: {marginTop: 5, marginBottom: 5}, disabled: true},
];

const toolbarControlProps: ToolbarControlProps = {
  tooltip: 'More formats',
  iconName: 'menu',
  showTooltip: true,
  includeDropdownArrow: true,
  width: 55,
};

const ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);

export default class MoreFormatDropdown extends React.Component {
  static defaultProps = {};
  props: MoreFormatDropdownProps;
  state: {};

  // TODO(joel) - make functional component
  shouldComponentUpdate(
    nextProps: MoreFormatDropdownProps,
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
        menuWidth={320}
        visible={this.props.visible}
        onSelect={item => ToolbarActionCreators.setFormat(item)}
        onOpen={() => ToolbarActionCreators.openItem('MoreFormat')}
        onClose={() => ToolbarActionCreators.closeItem('MoreFormat')}
      />
    );
  }
}
