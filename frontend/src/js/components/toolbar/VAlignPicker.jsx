/* @flow */

import React from 'react';
// $FlowFixMe
import pure from 'recompose/pure';

import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';
import GenIcon from './GenIcon';

import type {VAlignType} from '../../types/Eval';

import type {
  ToolbarControlProps,
  MenuProps,
} from '../../types/Toolbar';

type VAlignPickerProps = {
  value: VAlignType;
  visible: boolean;
};

const menuProps: Array<MenuProps> = [
  {
    tag: 'MenuItem',
    primaryText: 'Top',
    value: 'top',
    rightIcon: <GenIcon name="vertical_align_top" />,
  },
  {
    tag: 'MenuItem',
    primaryText: 'Middle',
    value: 'center',
    rightIcon: <GenIcon name="vertical_align_center" />,
  },
  {
    tag: 'MenuItem',
    primaryText: 'Bottom',
    value: 'bottom',
    rightIcon: <GenIcon name="vertical_align_bottom" />,
  },
];

const toolbarControlProps: ToolbarControlProps = {
  iconName: 'vertical_align_bottom',
  tooltip: 'Vertical align',
  includeDropdownArrow: true,
  showTooltip: true,
  spacing: 7,
  width: 41,
};

const ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);

function VAlignPicker(props: VAlignPickerProps): React.Element {
  const {visible, value} = props;
  return (
    <ButtonWithMenu
      toolbarControlProps={toolbarControlProps}
      menuProps={menuProps}
      menuWidth={65}
      visible={visible}
      value={value}
      onSelect={align => { /* TODO */ }}
      onOpen={() => ToolbarActionCreators.openItem('VAlignPicker')}
      onClose={() => ToolbarActionCreators.closeItem('VAlignPicker')}
    />
  );
}

export default pure(VAlignPicker);
