/* @flow */

import React, {PropTypes} from 'react';
import ASDropdownMenu from './ASDropdownMenu.jsx';
import Constants from '../../Constants';
import U from '../../AS/Util';

import type {
  ASLanguage
} from '../../types/Eval';

function languageIndex(lang: ASLanguage) {
  return Object.keys(Constants.Languages).indexOf(lang);
}

function languageChangeShortcut(lang: ASLanguage) {
  return U.Browser.metaKeyName() + "+" + String(languageIndex(lang) + 1);
}

type ASLanguageMenuPropsType = { 
  language: ASLanguage; 
  onSelectLanguage: (lang: ASLanguage) => void;
};

export default function ASLanguageMenu(props: ASLanguageMenuPropsType): React.Element {
  let languages = Object.keys(Constants.Languages).map((l) => {
    return {payload: l, text: l, shortcut: languageChangeShortcut(l)};
  });
  // the width of 129 is specified because material-ui ceilings by mod 64
  // and this is empirically the width we want
  return (
    <ASDropdownMenu
      autoWidth={false}
      width={129}
      selectedIndex={languageIndex(props.language)}
      menuItems={languages}
      onChange={props.onSelectLanguage}
      underlineStyle={{ display: 'none' }}
      {...props}
      />
  );
}