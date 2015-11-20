// Temporarily getting phased out in favor of an Eval Header file. (Alex 11/12)

import React from 'react';
const update = React.addons.update;
import ReplEditor from '../ReplEditor.jsx';
import Constants from '../../Constants.js';

import {AppBar, Toolbar, DropDownMenu, Styles, FlatButton} from 'material-ui';
let NavigationClose = require('material-ui/lib/svg-icons/navigation/close');


require('brace/mode/python');
require('brace/mode/r');
require('brace/mode/ocaml');
require('brace/theme/monokai');

/* TODO: We may not support all languages in the REPL */
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
      replLanguage: Constants.Languages.Python,
      theme: 'monokai'
    };
  },

  render() {

    return (
      <div style={{width:"100%",height:"100%",marginLeft:"6px"}} >
        <Toolbar
          style={{backgroundColor:Styles.Colors.grey900}}
          showMenuIconButton={false} >
          <DropDownMenu
            menuItems={languages}
            onChange={this.props.onReplLanguageChange}
            underlineStyle={{ display: 'none' }} />
        </Toolbar>
        <ReplEditor
          ref="editor" name="repl"
          onChange={function() {}}
          mode={this.props.replLanguage.Editor}
          language={this.props.replLanguage}
          theme="monokai"
          value={this.props.replValue}
          height="100%"
          onDeferredKey={this.props.onDeferredKey} />

      </div>
    );
  }

});






