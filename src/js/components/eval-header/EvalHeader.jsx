import React from 'react';
const update = React.addons.update;
import EvalHeaderEditor from '../EvalHeaderEditor.jsx';
import Constants from '../../Constants.js';

import {AppBar, Toolbar, DropDownMenu, Styles, FlatButton} from 'material-ui';
let NavigationClose = require('material-ui/lib/svg-icons/navigation/close');


require('brace/mode/python');
require('brace/mode/r');
require('brace/mode/ocaml');
require('brace/theme/monokai');

let languages = [];
for (var key in Constants.Languages) {
  languages.push({
    payload: Constants.Languages[key],
    text: Constants.Languages[key].Display
  });
}

export default React.createClass({

  getDefaultProps() {
    return {
      evalHeaderLanguage: Constants.Languages.Python,
      theme: 'monokai'
    };
  },

  render() {
    let languageInd = languages.map((l) => l.text).indexOf(this.props.evalHeaderLanguage.Display);
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
          mode={this.props.evalHeaderLanguage.Editor}
          language={this.props.evalHeaderLanguage}
          value={this.props.evalHeaderValue}
          height="100%"
          />
      </div>
    );
  }

});