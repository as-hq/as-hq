/* @flow */

import {logDebug} from '../AS/Logger';

import {evalHeaderEditor as zIndex} from '../styles/zIndex';
import CellStore from '../stores/ASCellStore';
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
  maxLines: ?number;
};

type EvalHeaderEditorProps = {
  mode: string;
  value: string;
  maxLines: ?number;
  language: ASLanguage;
  onChange: (xp: string) => void;
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
    this.editor = ace.edit(view_name);
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


  render(): React.Element {
    return (
      <div id={view_name}
           style={styles.root}
           onKeyDown={(e) => this._handleKeyDown(e)}
           onKeyUp={(e) => this._handleKeyUp(e)}>
      </div>);
  }

  _handleKeyDown(e: SyntheticKeyboardEvent) {
    if (U.Shortcut.evalHeaderShouldDeferKey(e)) {
      U.Key.killEvent(e);
      U.Shortcut.tryEvalHeaderShortcut(e);
    }
  }

  _handleKeyUp(e: SyntheticKeyboardEvent) {
    this.props.onChange(this.editor.getValue());
  }
}

const view_name = "EVAL_HEADER_EDITOR";

const styles = {
  root: {
    height: '100%'
  }
};

EvalHeaderEditor.defaultProps = {
  mode     : 'python',
  value    : '',
  maxLines : null
};
