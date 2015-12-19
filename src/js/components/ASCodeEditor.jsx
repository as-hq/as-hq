import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';
import Constants from '../Constants';

import {AppBar, Toolbar, ToolbarGroup, FlatButton, TextField, DropDownMenu, Styles} from 'material-ui';
import ASCellPropsToggleButton from './basic-controls/ASCellPropsToggleButton.jsx';
import ASCellPropsColorSetButton from './basic-controls/ASCellPropsColorSetButton.jsx';
import ASLanguageMenu from './basic-controls/ASLanguageMenu.jsx';

require('brace/mode/python');
require('brace/mode/r');
require('brace/mode/ocaml');
require('brace/mode/mysql');
require('brace/mode/java');
require('brace/mode/c_cpp');
require('brace/theme/monokai');

export default React.createClass({

  /*************************************************************************************************************************/
  // React methods

  propTypes: {
    onDeferredKey: React.PropTypes.func.isRequired
  },

  getDefaultProps() {
    return {
      language: Constants.Languages.Python,
      theme: 'monokai'
    };
  },

  // RE: the "SUBMIT BUG REPORT" button. It's temporary. It does not actually belong in
  // ASCodeEdtior (AT ALL!) but this is the simplest place to stick it for now. Also,

  /*************************************************************************************************************************/
  // Change handlers

  _onSelectLanguage(e, selectedIndex, menuItem) {
    //notify editor to change
    this.props.onSelectLanguage(menuItem); 
  },

  _onBlurVarName() {
    let varName = this.refs.varNameField.getValue();
    this.props.onSetVarName(varName);
  },

  /*************************************************************************************************************************/
  // Render

  render() {
    let {language, theme, value, width, height} = this.props;
    let mode = Constants.AceMode[language];

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
                fontFamily: '"Lucida Console", Monaco, monospace'
              }}
              floatingLabelStyle={{
                fontFamily: 'Roboto, sans-serif'
              }}
              onBlur={this._onBlurVarName} />
            <ASLanguageMenu 
              onSelectLanguage={this._onSelectLanguage}
              language={language}
              style={{
                marginTop: 'auto',
                marginLeft: '40px',
                marginBottom: 'auto'
            }} />
            <ASCellPropsToggleButton propTag="Bold" iconClassName="format_bold" style={{
              position: 'relative',
              marginLeft: '0px',
              top: '5px'
            }} />
            <ASCellPropsToggleButton propTag="Italic" iconClassName="format_italic" style={{
              position: 'relative',
              marginLeft: '0px',
              top: '5px'
            }} />
            <ASCellPropsColorSetButton propTag="TextColor" style={{
              position: 'relative',
              marginLeft: '20px'
            }} />
            <ASCellPropsColorSetButton propTag="FillColor" style={{
              position: 'relative',
              marginLeft: '0px'
            }} />
        </Toolbar>
        <AceEditor
          ref="editor"
          handleEditorFocus={this.props.handleEditorFocus}
          mode={mode}
          language={language}
          hideToast={this.props.hideToast}
          theme={theme}
          width={width} height={height}
          maxLines={this.props.maxLines}
          setFocus={this.props.setFocus}
          onDeferredKey={this.props.onDeferredKey} />
      </div>
    );
  },

  _onTest() {
    window.test();
  }
});
