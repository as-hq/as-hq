/* @flow */

import React from 'react';
// $FlowFixMe
import pure from 'recompose/pure';

import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

import type {
  ToolbarControlProps,
  MenuProps,
} from '../../types/Toolbar';

type FontSizePickerProps = {
  value: number;
  visible: boolean;
};

const fontSizes: Array<number> = [6, 7, 8, 9, 10, 11, 12, 14, 18, 24, 36];

const menuProps: Array<MenuProps> = fontSizes.map((size) => {
  return {tag: 'MenuItem', primaryText: `${size}`, value: `${size}`};
});

const toolbarControlProps: ToolbarControlProps = {
  tooltip: 'Font size',
  displayValue: '10',
  showTooltip: true,
  width: 45,
};

const ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);

function FontSizePicker(props: FontSizePickerProps): React.Element {
  const {visible, value} = props;
  return (
    <ButtonWithMenu
      menuShouldCheckSelections={false}
      toolbarControlProps={toolbarControlProps}
      menuProps={menuProps}
      menuWidth={65}
      toolbarControlWidth={45}
      visible={visible}
      value={value}
      onSelect={size => {/* TODO*/}}
      onOpen={() => ToolbarActionCreators.openItem('FontSizePicker')}
      onClose={() => ToolbarActionCreators.closeItem('FontSizePicker')}
    />
  );
}

export default pure(FontSizePicker);
