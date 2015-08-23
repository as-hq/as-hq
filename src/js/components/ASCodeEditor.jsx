import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';
import Constants from '../Constants';

import {AppBar, DropDownMenu, Styles} from 'material-ui';

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
    payload: Constants.Languages[key].Editor,
    text: Constants.Languages[key].Display
  });
}

console.log("languages: " + JSON.stringify(languages));

export default React.createClass({

  getDefaultProps() {
    return {
      language: Constants.Languages.Python,
      theme: 'monokai'
    };
  },

  render() {
    let {language, theme, value, width, height} = this.props;

    return (
      <div>
        <AppBar
          style={{
            backgroundColor: Styles.Colors.grey700
          }}
          showMenuIconButton={false} >
          <DropDownMenu
            menuItems={languages}
            onChange={this._onLanguageChange} />
        </AppBar>
        <AceEditor
          ref="editor"
          onChange={this.props.onExpressionChange}
          onEvalRequest={this.props.onEvalRequest}
          focusGrid={this.props.focusGrid}
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
    this.props.onLanguageChange(menuItem.payload);
  }
});
