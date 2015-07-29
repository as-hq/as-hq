import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';

import {AppBar, DropDownMenu, Styles} from 'material-ui';

require('brace/mode/python');
require('brace/mode/r');
require('brace/mode/ocaml');
require('brace/mode/mysql');
require('brace/mode/java');
require('brace/theme/monokai');

export default React.createClass({
  getDefaultProps() {
    return {
      mode: 'python',
      theme: 'monokai'
    };
  },

  render() {
    let {mode, theme, value, width, height} = this.props;
    let languages = ['Python', 'R', 'OCaml', 'SQL', 'Java'];

    return (
      <div>
        <AppBar
          style={{
            backgroundColor: Styles.Colors.grey700
          }}
          showMenuIconButton={false} >
          <DropDownMenu
            menuItems={
              languages.map((lang) => ({ payload: lang, text: lang }))
            } />
        </AppBar>
        <AceEditor mode={mode} theme={theme} value={value} width={width} height={height} />
      </div>
    );
  },

  _onActive(tab) {
    let lang = tab.props.label;
    //TODO
  }
});
