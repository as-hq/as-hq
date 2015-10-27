import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';
import Store from '../stores/ASEvaluationStore';
import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators.js';

var ace = require('brace');
var React = require('react');

// TODO: why is this actually needed after each prop change??
// also value isn't a prop anymore
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
    ExpStore.addChangeListener(this._onExpressionChange);
    this.editor = ace.edit(this.props.name);
    this.editor.$blockScrolling = Infinity;
    //this.editor.setValue('', 1);
    onPropsSet(this.editor, this.props);
  },

  componentWillUnmount(){
    ExpStore.removeChangeListener(this._onExpressionChange);
  },

  componentWillReceiveProps(nextProps) {
    console.log("ace editor receiving props", nextProps);
    onPropsSet(this.editor, nextProps);
  },

  /*************************************************************************************************************************/
  // Helpers

   getRawEditor() {
    return this.editor;
  },

  /*************************************************************************************************************************/
  // Handle events originating from ace editor

  _onKeyDown(e) {
    console.log("ACE KEYDOWN: " + e.which);
    if (ShortcutUtils.editorShouldDeferKey(e)) {
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    }
  },

  _onKeyUp(e){
    console.log("ACE KEYUP " + e.target.value);
    ExpActionCreator.handleEditorChange(e.target.value);
  },

  /*************************************************************************************************************************/
  // Respond to change events from ExpStore

   _onExpressionChange(){
    let xpOrigin = ExpStore.getXpOrigin();
    switch(xpOrigin){
      case Constants.xpChange.FROM_GRID:
        this.updateValue();
        break;
      case Contants.xpChange.FROM_TEXTBOX:
        this.updateValue();
        break;
      default:
        break;
    }
  },

  updateValue(){
    this.editor.setValue(ExpStore.getExpression());
  },

  /*************************************************************************************************************************/
  // Render

  render: function() {
    console.log("ace editor rendering");
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex: 0
    };
    return (<div
        id={this.props.name}
        style={divStyle}
        onKeyDown={this._onKeyDown}
        onKeyUp={this._onKeyUp}>
      </div>);
  }
});
