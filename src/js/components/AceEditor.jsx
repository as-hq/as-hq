import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';
import Util from '../AS/Util';
import ParseUtils from '../AS/ParsingUtils';

import Constants from '../Constants';

import Store from '../stores/ASEvaluationStore';
import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators.js';

var ace = require('brace');
var React = require('react');

function onPropsSet(editor, props) {
  editor.getSession().setMode('ace/mode/'+props.mode);
  editor.setTheme('ace/theme/'+props.theme);
  editor.setFontSize(props.fontSize);
  editor.renderer.setShowGutter(props.showGutter);
  editor.setOption('maxLines', props.maxLines);
  editor.setOption('readOnly', props.readOnly);
  editor.setOption('highlightActiveLine', props.highlightActiveLine);
  editor.setShowPrintMargin(props.setShowPrintMargin);
  if (props.onLoad) {
    props.onLoad(editor);
  }
}

module.exports = React.createClass({

  /*************************************************************************************************************************/
  // React methods

  propTypes: {
    handleEditorFocus: React.PropTypes.func.isRequired,
    onDeferredKey: React.PropTypes.func.isRequired,
    mode  : React.PropTypes.string,
    theme : React.PropTypes.string,
    name : React.PropTypes.string,
    height : React.PropTypes.string,
    width : React.PropTypes.string,
    fontSize : React.PropTypes.number,
    showGutter : React.PropTypes.bool,
    onLoad: React.PropTypes.func,
    maxLines : React.PropTypes.number,
    readOnly : React.PropTypes.bool,
    highlightActiveLine : React.PropTypes.bool,
    showPrintMargin : React.PropTypes.bool,
    sendBackExpression : React.PropTypes.func
  },

  getDefaultProps() {
    return {
      name   : 'brace-editor',
      mode   : 'python',
      theme  : 'monokai',
      height : '100px',
      width  : '100%',
      fontSize   : 12,
      showGutter : true,
      onLoad     : null,
      maxLines   : null,
      readOnly   : false,
      highlightActiveLine : true,
      showPrintMargin     : true,
      sendBackExpression : null
    };
  },

  componentDidMount() {
    // Respond to changes from ExpStore, focus, changes, and keydowns
    ExpStore.addChangeListener(this._onExpressionChange);
    this.editor = ace.edit(this.props.name);
    this.editor.$blockScrolling = Infinity;
    this.editor.on('focus', this._onFocus);
    this.editor.getSession().on('change', this._onChange);
    this.editor.container.addEventListener('keydown',this._onKeyDown,true);
    this.editor.setOptions({
     enableBasicAutocompletion: true
    });
    onPropsSet(this.editor, this.props);
  },

  componentWillUnmount(){
    ExpStore.removeChangeListener(this._onExpressionChange);
  },

  /*************************************************************************************************************************/
  // Helpers

  getRawEditor() {
    return this.editor;
  },

  insertRef(newRef){
    let lastRef = ExpStore.getLastRef();
    console.log("Inserting ref in editor " + newRef);
    ExpStore.setDoEditorCallback(false);
    if (lastRef){
      ParseUtils.deleteLastRef(this.editor,lastRef)
    }
    this.editor.getSession().insert(this.editor.getCursorPosition(),newRef);
    console.log("New editor xp: " + this.editor.getValue());
  },

  /*************************************************************************************************************************/
  // Handle events originating from ace editor

  /* 
  In the ace editor, all nav keys are internal (don't do selection in grid, ever)
    In  particular, Nav keys aren't deferred
  Only defer things like Ctrl Enter
  If we haven't deferred, then the editor should go to the callback (Action Creator) 
  when onChange fires (right after this)
  */
  _onKeyDown(e) {
    console.log("\n\nACE KEYDOWN");
    if (ShortcutUtils.editorShouldDeferKey(e)) {
      // Try shortcut in eval pane
      console.log("Deferring editor key down");
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    }
    else {
      ExpStore.setDoEditorCallback(true);
    }
  },

  /* 
  Note: One of the reasons we want onChange and not keyUp is so that pressing
  backspace for a long time in the editor actually makes the textbox update real-time (multiple onChanges fired, only one keyUp fired)
  
  This methods fires on Ace's onChange; for example after editor.setValue. 
  This is different from the outer div's onChange (which doesn't have a callback)
  If you don't want the callback (action creator) to fire, need to set doAceCallback in ExpStore to false
  Since keydown sets this to true, events originating within ace will be sent to the callback (action creator)
  */
  _onChange(e){
    let xpStr = this.editor.getValue();
    if (ExpStore.getDoEditorCallback()){
      console.log("Ace editor detected keychange with callback: " + xpStr);
      ExpActionCreator.handleEditorChange(xpStr);
    }
  },

  /* 
  When the editor receives focus, notify the stores
  */
  _onFocus(e){
    console.log("The editor now has focus");
    console.assert(ExpStore.getUserIsTyping());
    Store.setFocus('editor');
    ExpStore.setLastCursorPosition(Constants.CursorPosition.EDITOR);
    ExpStore.setLastRef(null); 
    this.props.handleEditorFocus();
  },

  /*************************************************************************************************************************/
  // Respond to change events from ExpStore

  /*
  Case on the origin in the ExpStore, which is the reason for this expression update
  Most of the cases just call updateValue
  */
  _onExpressionChange(){
    let xpChangeOrigin = ExpStore.getXpChangeOrigin();
    switch(xpChangeOrigin){
      case Constants.ActionTypes.GRID_KEY_PRESSED:
        console.log("Ace editor caught GRID type update");
        this.updateValue();
        break;
      case Constants.ActionTypes.TEXTBOX_CHANGED:
        console.log("Ace editor caught TEXTBOX type update");
        this.updateValue();
        break;
      case Constants.ActionTypes.NORMAL_SEL_CHANGED:
        console.log("Ace editor caught SEL_CHNG type update");
        this.updateValue();
        break;
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID:
        console.log("Ace editor caught PARTIAL GRID");
        this.updateValue();
        break;
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_TEXTBOX:
        console.log("Ace editor caught PARTIAL TEXTBOX");
        this.updateValue();
        break;
      case Constants.ActionTypes.ESC_PRESSED:
        console.log("Ace editor caught ESC");
        this.updateValue();
        break;
      default: 
        // don't need to do anything on EDITOR_CHANGED
        // or PARTIAL_REF_CHANGE_WITH_EDITOR
        break;
    }
  },

  /* 
  Used by the action creator callbacks to update the editor
  Don't do a onChange callback; as this request is external (due to Action Creator), not from the editor itself
  Just set value and unselect 
  */
  updateValue(){
    ExpStore.setDoEditorCallback(false);
    this.editor.setValue(ExpStore.getExpression());
    this.editor.clearSelection(); // otherwise ace highlights whole xp
  },

  /*************************************************************************************************************************/
  // Render

  render: function() {
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex: 0,
      resize: 'both'
    };
    return (<div id={this.props.name} style={divStyle} />);
  }

});
