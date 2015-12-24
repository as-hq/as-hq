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
      theme: 'monokai'
    };
  },

  /*************************************************************************************************************************/
  // Render

  render() {
    let {theme, value, width, height} = this.props;
    return (
      <div>
        <Toolbar/>
        <AceEditor
          ref="editor"
          handleEditorFocus={this.props.handleEditorFocus}
          hideToast={this.props.hideToast}
          theme={theme}
          width={width} 
          height={height}
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
