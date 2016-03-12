/* @flow */

import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';

import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

import ExpressionActions from '../../actions/ASExpressionActionCreators';
import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';

import U from '../../AS/Util';

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
  {name: 'Excel', shortcut: U.Browser.metaKeyName()+'+1'},
  {name: 'Python', shortcut: U.Browser.metaKeyName()+'+2'},
  {name: 'R', shortcut: U.Browser.metaKeyName()+'+3'},
  {name: 'SQL', shortcut: U.Browser.metaKeyName()+'+4'},
];

const menuProps: Array<MenuProps> = languages.map(({name, shortcut}) => ({
  tag: 'MenuItem',
  primaryText: name,
  secondaryText: shortcut,
  value: name,
}));

const ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);

export default class LanguagePicker {
  static defaultProps: {} = {}; 
  props: LanguagePickerProps; 
  state: {}; 

  shouldComponentUpdate(nextProps: LanguagePickerProps, nextState: {}): boolean { 
    return shallowCompare(this, nextProps, nextState);
  }

  render(): React.Element {
    const {language, visible} = this.props;
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
        onClose={() => ToolbarActionCreators.closeItem('LanguagePicker')} />
    );
  }
}
