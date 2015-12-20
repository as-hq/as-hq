import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';
import Constants from '../Constants';
import Toolbar from './toolbar/Toolbar.jsx';

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
        <Toolbar />
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
