/* @flow */

import React from 'react';
import EvalHeaderEditor from '../EvalHeaderEditor.jsx';
import Constants from '../../Constants.js';

// $FlowFixMe
import {AppBar, Toolbar, DropDownMenu, Styles, FlatButton} from 'material-ui';
// $FlowFixMe
let NavigationClose = require('material-ui/lib/svg-icons/navigation/close');

import API from '../../actions/ASApiActionCreators';

import type {
  ASLanguage
} from '../../types/Eval';

// $FlowFixMe
require('brace/mode/python');
// $FlowFixMe
require('brace/mode/r');
// $FlowFixMe
require('brace/mode/ocaml');
// $FlowFixMe
require('brace/theme/monokai');

let languages = []; 

for (var key in Constants.Languages) {
  languages.push({
    payload: Constants.Languages[key],
    text: Constants.Languages[key]
  });
}

type EvalHeaderDefaultProps = {
  evalHeaderLanguage: ASLanguage; 
};

type EvalHeaderProps = {
  evalHeaderLanguage: ASLanguage; 
  evalHeaderValue: string; 
  onSubmitEvalHeader: () => void; 
  // $FlowFixMe no idea why ASLanguage is incompatible with string literals...
  onEvalHeaderLanguageChange: (e: {}, index: number, menuItem: { payload: ASLanguage }) => void; 
};

type EvalHeaderState = {};

export default class EvalHeader
  extends React.Component<EvalHeaderDefaultProps, EvalHeaderProps, EvalHeaderState>
{
  constructor(props: EvalHeaderDefaultProps) { 
    super(props); 
  }

  saveAndEval() { 
    API.evaluateHeader(this.refs.editor.getRawEditor().getValue(), this.props.evalHeaderLanguage);
  }

  render(): React.Element {
    let languageInd = languages.map((l) => l.text).indexOf(this.props.evalHeaderLanguage);
    return (
      <div style={{width:"100%",height:"100%",marginLeft:"6px"}}>
        <Toolbar
          style={{backgroundColor:Styles.Colors.grey900}}
          showMenuIconButton={false}>
          <DropDownMenu
            menuItems={languages}
            onChange={this.props.onEvalHeaderLanguageChange}
            selectedIndex={languageInd}
            underlineStyle={{ display: 'none' }} />
          <FlatButton 
            label="Save"
            style={{
                position: 'relative',
                left: '55px',
                top: '-10px',
                fontFamily: '"Lucida Console", Monaco, monospace'
            }}
            onClick={this.props.onSubmitEvalHeader} />
        </Toolbar>
        <EvalHeaderEditor
          ref="editor" name="evalHeader"
          saveAndEval={this.saveAndEval.bind(this)}
          mode={Constants.AceMode[this.props.evalHeaderLanguage]}
          language={this.props.evalHeaderLanguage}
          value={this.props.evalHeaderValue}
          height="100%" />
      </div>
    );
  }
}

EvalHeader.defaultProps = { 
  evalHeaderLanguage: Constants.Languages.Python,
};