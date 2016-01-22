/* @flow */

import React from 'react';
import pure from 'recompose/pure';

import ExpActionCreators from '../../actions/ASExpActionCreators';
import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';
import GenIcon from './GenIcon';

import type {HAlignType} from '../../types/Eval';

import type {
  ToolbarControlProps,
  MenuProps,
} from '../../types/Toolbar';

type HAlignPickerProps = {
  value: HAlignType;
  visible: boolean;
};

const menuProps: Array<MenuProps> = [
  {
    tag: 'MenuItem',
    primaryText: 'Left',
    value: 'left',
    rightIcon: <GenIcon name="format_align_left" />,
  },
  {
    tag: 'MenuItem',
    primaryText: 'Center',
    value: 'center',
    rightIcon: <GenIcon name="format_align_center" />,
  },
  {
    tag: 'MenuItem',
    primaryText: 'Right',
    value: 'right',
    rightIcon: <GenIcon name="format_align_right" />,
  },
];

const toolbarControlProps: ToolbarControlProps = {
  iconName: 'format_align_left',
  tooltip: 'Horizontal align',
  includeDropdownArrow: true,
  showTooltip: true,
  spacing: 7,
  width: 41,
};

const ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);

function HAlignPicker(props: HAlignPickerProps): React.Element {
  const {visible, value} = props;
  return (
    <ButtonWithMenu
      toolbarControlProps={toolbarControlProps}
      menuProps={menuProps}
      menuWidth={65}
      visible={visible}
      value={value}
      onSelect={align => ExpActionCreators.setVAlign(align)}
      onOpen={() => ToolbarActionCreators.openItem('VAlignPicker')}
      onClose={() => ToolbarActionCreators.closeItem('VAlignPicker')}
    />
  );
}

export default pure(HAlignPicker);
