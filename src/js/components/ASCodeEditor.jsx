import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';

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
    return (
      <AceEditor mode={mode} theme={theme} value={value} width={width} height={height} />
    );
  }
});
