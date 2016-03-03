/* @flow */

import type {
  ToolbarControlProps,
} from '../../types/Toolbar';

import React from 'react';
// $FlowFixMe
import pure from 'recompose/pure';

import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

type FontPickerProps = {
  value: string;
  visible: boolean;
}

const fonts = [
  {name: 'Arial', family: 'Arial'},
  {name: 'Times New Roman', family: 'Times New Roman'},
];

const menuItems = fonts.map(font => ({
  tag: 'MenuItem',
  primaryText: font.name,
  style: {fontFamily: font.family},
  value: font.name,
}));

const toolbarControlProps: ToolbarControlProps = {
  displayValue: 'Arial',
  tooltip: 'Fonts',
  showTooltip: true,
  width: 200,
};

const ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);

function FontPicker(
  props: FontPickerProps
): React.Element {
  const {visible, value} = props;

  return (
    <ButtonWithMenu
      toolbarControlProps={toolbarControlProps}
      menuProps={menuItems}
      menuWidth={65}
      toolbarControlWidth={200}
      visible={visible}
      value={value}
      onSelect={font => { /* TODO */ }}
      onOpen={() => ToolbarActionCreators.openItem('FontPicker')}
      onClose={() => ToolbarActionCreators.closeItem('FontPicker')}
    />
  );
}

export default pure(FontPicker);
