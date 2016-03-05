/* @flow */

import React from 'react';

import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

import ExpressionActions from '../../actions/ASExpressionActionCreators';
import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import type {
  MenuProps,
} from '../../types/Toolbar';

import type {
  ASLanguage,
} from '../../types/Eval';

type LanguageMenuOption = {
  name: string;
  shortcut: string;
};

type LanguagePickerProps = {
  visible: boolean;
  language: ASLanguage;
};

const languages: Array<LanguageMenuOption> = [
  {name: 'Excel', shortcut: 'Ctrl+1'},
  {name: 'Python', shortcut: 'Ctrl+2'},
  {name: 'R', shortcut: 'Ctrl+3'},
  {name: 'SQL', shortcut: 'Ctrl+4'},
];

const menuProps: Array<MenuProps> = languages.map(({name, shortcut}) => ({
  tag: 'MenuItem',
  primaryText: name,
  secondaryText: shortcut,
  value: name,
}));

const ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);

export default function LanguagePicker(
  props: LanguagePickerProps
): React.Element {
  const {language, visible} = props;
  const toolbarControlProps = {
    displayValue: language,
    tooltip: 'Languages',
    showTooltip: true,
    width: 85,
  };

  return (
    <ButtonWithMenu
      menuShouldCheckSelections={false}
      toolbarControlProps={toolbarControlProps}
      menuProps={menuProps}
      menuWidth={165}
      toolbarControlWidth={85}
      visible={visible}
      value={language}
      onSelect={lang => ExpressionActions.setLanguage(lang)}
      onOpen={() => ToolbarActionCreators.openItem('LanguagePicker')}
      onClose={() => ToolbarActionCreators.closeItem('LanguagePicker')}
    />
  );
}
