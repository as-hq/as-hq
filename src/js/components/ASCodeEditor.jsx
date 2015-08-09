import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';

import {AppBar, DropDownMenu, Styles} from 'material-ui';

require('brace/mode/python');
require('brace/mode/r');
require('brace/mode/ocaml');
require('brace/mode/mysql');
require('brace/mode/java');
require('brace/mode/c_cpp');
require('brace/theme/monokai');

let languages = [
  { payload: 'python', text: 'Python' },
  { payload: 'r', text: 'R' },
  { payload: 'ocaml', text: 'OCaml' },
  { payload: 'mysql', text: 'SQL' },
  { payload: 'c_cpp', text: 'C++' },
  { payload: 'java', text: 'Java' }
];

export default React.createClass({
  getDefaultProps() {
    return {
      mode: 'python',
      theme: 'monokai'
    };
  },

  render() {
    let {mode, theme, value, width, height} = this.props;

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
        <AceEditor ref="editor"
          onChange={this.props.onExpressionChange}
          requestEval={this.props.onEvalRequest}
          focusGrid={this.props.focusGrid}
          mode={mode}
          theme={theme}
          value={value}
          width={width} height={height} />
      </div>
    );
  },

  _onLanguageChange(e, selectedIndex, menuItem) {
    //notify editor to change
    this.props.onLanguageChange(menuItem.payload);
  }
});
