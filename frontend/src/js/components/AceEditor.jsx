/* @flow */

import {logDebug} from '../AS/Logger';

import U from '../AS/Util';

import Constants from '../Constants';

import CellStore from '../stores/ASCellStore';
import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators.js';

import type {
  ASFocusType
} from '../types/State';

import type {
  ASLanguage
} from '../types/Eval';

// $FlowFixMe somehow Flow isn't recognizing these?
import ace from 'brace';
import React from 'react';

// side-effectful import needed to enable autocomplete
// $FlowFixMe somehow Flow isn't recognizing these?
import 'brace/ext/language_tools';

function onPropsSet(editor, props) {
  editor.setTheme('ace/theme/' + props.theme);
  // editor.setFontSize(props.fontSize);
  editor.renderer.setShowGutter(props.showGutter);
  editor.setOption('maxLines', props.maxLines);
  editor.setOption('minLines', props.minLines);
  editor.setOption('readOnly', props.readOnly);
  editor.setOption('highlightActiveLine', props.highlightActiveLine);
  editor.setShowPrintMargin(props.showPrintMargin);
  if (props.onLoad) {
    props.onLoad(editor);
  }
}

type AceEditorProps = {
  handleEditorFocus: () => void;
  onDeferredKey: (e: SyntheticKeyboardEvent) => void;
  hideToast: () => void;
  setFocus: (elem: ASFocusType) => void;
  theme: string;
  name: string;
  height: string;
  width: string;
  // fontSize: number; (Anand made this font larger; restoring it back to original for now. Alex 12/30)
  showGutter: boolean;
  onLoad: ?((editor: any) => void);
  minLines: number;
  maxLines: number;
  readOnly: boolean;
  highlightActiveLine: boolean;
  showPrintMargin: boolean;
};

type AceEditorDefaultProps = {
  name: string;
  theme: string;
  height: string;
  width: string;
  // fontSize: number;
  showGutter: boolean;
  onLoad: ?((editor: any) => void);
  minLines: number;
  maxLines: number;
  readOnly: boolean;
  highlightActiveLine: boolean;
  showPrintMargin: boolean;
};

type AceEditorState = {
  language: ASLanguage;
};

export default class AceEditor
  extends React.Component<AceEditorDefaultProps, AceEditorProps, AceEditorState>
{
  editor: any;

  /*************************************************************************************************************************/
  // React methods

  constructor(props: AceEditorProps) {
    super(props);

    this.state = {
      language: Constants.Languages.Excel
    }
  }

  componentDidMount() {
    // Respond to changes from ExpStore, focus, changes, and keydowns
    ExpStore.addChangeListener(this._onExpressionChange.bind(this));
    this.editor = ace.edit(this.props.name);
    this.editor.$blockScrolling = Infinity;
    this.editor.on('focus', this._onFocus.bind(this));
    this.editor.getSession().on('change', this._onChange.bind(this));
    this.editor.container.addEventListener('keydown', this._onKeyDown.bind(this), true);
    this.editor.setOptions({
      enableBasicAutocompletion: true
    });
    this._updateMode(this.state.language);
    onPropsSet(this.editor, this.props);
  }

  // When the component is about to update (after the initial render), update the mode of the editor as well
  // This has the effect of enabling autocomplete/syntax highlighting for that language, among other things
  componentWillUpdate(nextProps: AceEditorProps, nextState: AceEditorState) {
    if (nextState.language !== this.state.language) {
      this._updateMode(nextState.language);
    }
  }

  _updateMode(lang: ASLanguage) {
    if (this.editor) {
      this.editor.getSession().setMode('ace/mode/' + Constants.AceMode[lang]);
    }
  }

  componentWillUnmount() {
    ExpStore.removeChangeListener(this._onExpressionChange.bind(this));
  }

  componentWillReceiveProps(nextProps: AceEditorProps) {
    onPropsSet(this.editor, nextProps);
  }

  /*************************************************************************************************************************/
  // Helpers

  getRawEditor(): any {
    return this.editor;
  }

  insertRef(newRef: string) {
    let lastRef = ExpStore.getLastRef();
    logDebug("Inserting ref in editor " + newRef);
    ExpStore.setDoEditorCallback(false);
    if (lastRef) U.Parsing.deleteLastRef(this.editor,lastRef)
    this.editor.insert(newRef);
  }

  /*************************************************************************************************************************/
  // Handle events originating from ace editor

  /*
  In the ace editor, all nav keys are internal (don't do selection in grid, ever)
    In  particular, Nav keys aren't deferred
  Only defer things like Ctrl Enter
  If we haven't deferred, then the editor should go to the callback (Action Creator)
  when onChange fires (right after this)
  */
  _onKeyDown(e: SyntheticKeyboardEvent) {
    logDebug("\n\nACE KEYDOWN");
    if (U.Shortcut.editorShouldDeferKey(e)) {
      // Try shortcut in eval pane
      logDebug("Deferring editor key down");
      U.Key.killEvent(e);
      this.props.onDeferredKey(e);
    } else {
      ExpStore.setDoEditorCallback(true);
    }
  }

  /*
  Note: One of the reasons we want onChange and not keyUp is so that pressing
  backspace for a long time in the editor actually makes the textbox update real-time (multiple onChanges fired, only one keyUp fired)

  This methods fires on Ace's onChange; for example after editor.setValue.
  This is different from the outer div's onChange (which doesn't have a callback)
  If you don't want the callback (action creator) to fire, need to set doAceCallback in ExpStore to false
  Since keydown sets this to true, events originating within ace will be sent to the callback (action creator)
  */
  _onChange(e: SyntheticKeyboardEvent) {
    let xpStr = this.editor.getValue();
    if (ExpStore.getDoEditorCallback()) {
      logDebug("Ace editor detected keychange with callback: " + xpStr);
      ExpActionCreator.handleEditorChange(xpStr);
    }
  }

  /*
  When the editor receives focus, notify the stores
  */
  _onFocus(e: SyntheticKeyboardEvent) {
    logDebug("The editor now has focus");
    this.props.setFocus('editor');
    this.props.hideToast();
    ExpStore.setLastCursorPosition(Constants.CursorPosition.EDITOR);
    ExpStore.setLastRef(null);
    this.props.handleEditorFocus();
  }

  /*************************************************************************************************************************/
  // Respond to change events from ExpStore

  /*
  Case on the origin in the ExpStore, which is the reason for this expression update
  Most of the cases just call updateValue and update the language as well
  */
  _onExpressionChange() {
    let xpChangeOrigin = ExpStore.getXpChangeOrigin();
    logDebug("Editor caught exp update of_type: ", xpChangeOrigin);

    switch (xpChangeOrigin) {
      case Constants.ActionTypes.GRID_KEY_PRESSED:
      case Constants.ActionTypes.TEXTBOX_CHANGED:
      case Constants.ActionTypes.NORMAL_SEL_CHANGED:
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID:
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_TEXTBOX:
      case Constants.ActionTypes.ESC_PRESSED:
      case Constants.ActionTypes.BACKEND_UPDATED_AND_CELLS_CHANGED:
        this.updateValue();
        if (ExpStore.getLanguage() !== this.state.language) {
          this.setState({language: ExpStore.getLanguage()});
        }
        break;
      // When the user toggles the language by shortcut/toolbar, update language state, which will also update mode
      case Constants.ActionTypes.LANGUAGE_TOGGLED:
        this.setState({language: ExpStore.getLanguage()});
        break;
      default:
        // don't need to do anything on EDITOR_CHANGED
        break;
    }
  }

  /*
  Used by the action creator callbacks to update the editor
  Don't do a onChange callback; as this request is external (due to Action Creator), not from the editor itself
  Just set value and unselect
  */
  updateValue() {
    logDebug("Expression: " + ExpStore.getExpression());
    ExpStore.setDoEditorCallback(false);
    this.editor.setValue(ExpStore.getExpression());
    this.editor.clearSelection(); // otherwise ace highlights whole xp
  }

  /*************************************************************************************************************************/
  // Render

  render(): React.Element {
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex: 0,
      resize: 'both',
      overflow:'auto'
    };
    return (<div id={this.props.name} style={divStyle} />);
  }

}

AceEditor.propTypes = {
  name : React.PropTypes.string,
  height : React.PropTypes.string,
  width : React.PropTypes.string,
  // fontSize : React.PropTypes.number,  (Anand made this font larger; restoring it back to original for now. Alex 12/30)
  showGutter : React.PropTypes.bool,
  onLoad: React.PropTypes.func,
  minLines : React.PropTypes.number,
  maxLines : React.PropTypes.number,
  readOnly : React.PropTypes.bool,
  highlightActiveLine : React.PropTypes.bool,
  showPrintMargin : React.PropTypes.bool,
}

AceEditor.defaultProps = {
  name   : 'brace-editor',
  theme  : 'monokai',
  height : '100px',
  width  : '100%',
  // fontSize   : 25,  (Anand made this font larger; restoring it back to original for now. Alex 12/30)
  showGutter : true,
  onLoad     : null,
  // These props say that the editor starts at minLines lines
  // and grow until maxLines, at which point it starts scrolling
  maxLines   : 10,
  minLines   : 3,
  readOnly   : false,
  highlightActiveLine : true,
  showPrintMargin     : true,
}
