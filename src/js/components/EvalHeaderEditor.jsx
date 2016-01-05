/* @flow */

import {logDebug} from '../AS/Logger';

import CellStore from '../stores/ASCellStore';
import EvalHeaderActionCreator from '../actions/ASEvalHeaderActionCreators';
import API from '../actions/ASApiActionCreators';

import U from '../AS/Util';

import type {
  ASLanguage
} from '../types/Eval';

// $FlowFixMe
var ace = require('brace');
var React = require('react');

const defaultEditorProps = { 
  theme  : 'monokai',
  fontSize   : 12,
  showGutter : true,
  readOnly   : false,
  highlightActiveLine : true,
  showPrintMargin     : true,
};

function onPropsSet(editor, props) {
  editor.getSession().setMode('ace/mode/'+props.mode);
  editor.setOption('maxLines', props.maxLines);

  editor.setTheme('ace/theme/'+defaultEditorProps.theme);
  editor.setFontSize(defaultEditorProps.fontSize);
  editor.renderer.setShowGutter(defaultEditorProps.showGutter);
  
  editor.setOption('readOnly', defaultEditorProps.readOnly);
  editor.setOption('highlightActiveLine', defaultEditorProps.highlightActiveLine);
  editor.setShowPrintMargin(defaultEditorProps.showPrintMargin);

  editor.getSession().setUseSoftTabs(false);
}

type EvalHeaderEditorDefaultProps = {
  mode: string; 
  value: string; 
  height: string; 
  width: string; 
  name: string; 
  maxLines: ?number; 
};

type EvalHeaderEditorProps = {
  mode: string; 
  value: string; 
  height: string; 
  width: string; 
  name: string; 
  maxLines: ?number; 
  language: ASLanguage; 
  saveAndEval: () => void; 
};

type EvalHeaderEditorState = {};

export default class EvalHeaderEditor
  extends React.Component<EvalHeaderEditorDefaultProps, EvalHeaderEditorProps, EvalHeaderEditorState>
{
  /***************************************************************************************************************************/
  // React methods

  editor: any; 

  constructor(props: EvalHeaderEditorDefaultProps) {
    super(props);
  }

  getRawEditor(): any {
    return this.editor; 
  }

  componentDidMount() {
    this.editor = ace.edit(this.props.name);
    this.editor.$blockScrolling = Infinity;
    this.editor.setValue(this.props.value, 1);
    onPropsSet(this.editor, this.props);
  }

  componentWillReceiveProps(nextProps: EvalHeaderEditorProps) {
    if (this.editor.getValue() !== nextProps.value) {
      this.editor.setValue(nextProps.value, 1);
    }
    onPropsSet(this.editor, nextProps);
  }

  handleKeyDown(e: SyntheticKeyboardEvent) {
    let lang = this.props.language,
        val = this.editor.getValue();
    if (U.Shortcut.evalHeaderShouldDeferKey(e)) {
      U.Key.killEvent(e);
      U.Shortcut.tryShortcut(e, 'evalHeader');
    }
  }

  handleKeyUp(e: SyntheticKeyboardEvent) {
    let lang = this.props.language,
        val = this.editor.getValue();
    EvalHeaderActionCreator.storeEvalHeaderExpression(lang, val);
  }

  render(): React.Element {
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex: 0
    };
    return (<div
        id={this.props.name}
        style={divStyle}
        onKeyDown={this.handleKeyDown.bind(this)}
        onKeyUp={this.handleKeyUp.bind(this)}>
      </div>);
  }
}

EvalHeaderEditor.propTypes = {
  mode     : React.PropTypes.string,
  value    : React.PropTypes.string,
  height   : React.PropTypes.string,
  width    : React.PropTypes.string, 
  maxLines : React.PropTypes.number,
  language : React.PropTypes.string
};

EvalHeaderEditor.defaultProps = { 
  mode     : 'python',
  value    : '',
  height   : '100px',
  width    : '100%',
  name     : 'brace-editor',
  maxLines : null
};