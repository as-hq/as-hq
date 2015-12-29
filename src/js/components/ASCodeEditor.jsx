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

const ASCodeEditor = React.createClass({

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
    const {theme, value, width, height} = ASCodeEditor.props;
    const outerStyle = {
      display: 'flex',
      flexDirection: 'column',
      flexGrow: 0,
      flexShrink: 0,
      flexBasis: 'auto'
    };

    return (
      <div style={outerStyle}>
        <Toolbar />
        <AceEditor
          ref="editor"
          handleEditorFocus={ASCodeEditor.props.handleEditorFocus}
          hideToast={ASCodeEditor.props.hideToast}
          theme={theme}
          width="100%"
          height="100%"
          maxLines={ASCodeEditor.props.maxLines}
          setFocus={ASCodeEditor.props.setFocus}
          onDeferredKey={ASCodeEditor.props.onDeferredKey} />
      </div>
    );
  },

  _onTest() {
    window.test();
  }
});

export default ASCodeEditor;