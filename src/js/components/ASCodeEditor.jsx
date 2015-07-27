import React from 'react';
import brace from 'brace';
import AceEditor from 'react-ace';
import ActionCreator from '../actions/ASCodeEditorActionCreators';

let _langs = ['python', 'java', 'mysql', 'ruby', 'cpp', 'ocaml'];
_langs.forEach(lang => require('brace/mode/' + lang));

require('brace/theme/monokai');

export default React.createClass({
  getDefaultProps() {
    return {
      mode: 'python',
      theme: 'monokai'
    };
  },

  render() {
    let {mode, theme, value} = this.props;
    return (
      <AceEditor mode={mode} theme={theme} value={value} />
    );
  }
});
