import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';
import Constants from '../Constants';

import {AppBar, Toolbar, ToolbarGroup, FlatButton, TextField, DropDownMenu, Styles} from 'material-ui';

require('brace/mode/python');
require('brace/mode/r');
require('brace/mode/ocaml');
require('brace/mode/mysql');
require('brace/mode/java');
require('brace/mode/c_cpp');
require('brace/theme/monokai');

let languages = [];
for (var key in Constants.Languages) {
  languages.push({
    payload: Constants.Languages[key],
    text: Constants.Languages[key].Display
  });
}

function languageIndex(lang) {
  return languages.map((l) => l.text).indexOf(lang.Display);
}

export default React.createClass({

  getDefaultProps() {
    return {
      language: Constants.Languages.Python,
      theme: 'monokai'
    };
  },

  render() {
    let {language, theme, value, width, height} = this.props;
    // console.log("CODE EDITOR HEIGHT, WIDTH: " + height +  " " + width);

    return (
      <div>
      <Toolbar
              style={{backgroundColor: Styles.Colors.grey700, height:'60px'}}
              showMenuIconButton={false} >
                <TextField
                  ref="varNameField"
                  hintText="varName"
                  style={{
                    position: 'relative',
                    left: '-5px',
                    top: '-15px',
                    fontFamily: '"Lucida Console", Monaco, monospace'
                  }}
                  floatingLabelStyle={{
                    fontFamily: 'Roboto, sans-serif'
                  }}
                  onBlur={this._onBlurVarName} />
                <DropDownMenu
                  selectedIndex={languageIndex(language)}
                  menuItems={languages}
                  onChange={this._onLanguageChange}
                  underlineStyle={{ display: 'none' }}
                  style={{
                    position: 'relative',
                    left: '40px',
                    top: '0px'
                  }}/>
                <FlatButton
                  label="REPL"
                  onClick={this.props.onReplClick}
                  style={{
                    position: 'relative',
                    left: '40px',
                    top: '-13px'
                  }} />
            </Toolbar>
        <AceEditor
          ref="editor"
          onChange={this.props.onExpressionChange}
          mode={language.Editor}
          language={language}
          theme={theme}
          value={value}
          width={width} height={height}
          onDeferredKey={this.props.onDeferredKey} />
      </div>
    );
  },


  _onLanguageChange(e, selectedIndex, menuItem) {
    //notify editor to change
    console.log("language changed in ascodeeditor: " + JSON.stringify(menuItem.payload));
    this.props.onLanguageChange(menuItem.payload);
  },

  _onBlurVarName() {
    let varName = this.refs.varNameField.getValue();
    this.props.onSetVarName(varName);
  }
});
