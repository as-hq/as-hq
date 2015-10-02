import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';

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
  if (props.isRepl){
    editor.getSession().setUseSoftTabs(false);
    /* Deal with >>> readonly for repl */
    editor.container.addEventListener('keydown', function(e) {
        let key = e.keyCode || e.charCode;
        let pos = editor.getCursorPosition();
        let line = editor.getSession().getLine(pos.row);
        if (editor.selection.getCursor().column < 4 ){
          e.preventDefault();
          e.stopPropagation();
          return;
        }
        else if (editor.selection.getCursor().column == 4 ){ // begin line cases
          console.log(pos);
          if( key == 8 || key == 37){ // backspace,
            e.preventDefault();
            e.stopPropagation();
            // console.log(JSON.stringify(line));
            let isAtPrompt = line.substring(0,4) === ">>> ";
            if (!isAtPrompt) {
              if (key == 8){
                editor.removeToLineStart();
                editor.gotoLine(pos.row , Infinity);
              }
              else if (pos.row != 0) {
                editor.gotoLine(pos.row , Infinity);
              }
            }
          }
        } else if (key == 39) {
          // console.log("forward key");
          // console.log("row length: " + line.length + ", col: " + pos.column);
          if (pos.column === line.length) {
            // console.log("eol key triggered");
            e.preventDefault();
            e.stopPropagation();
            editor.gotoLine(pos.row + 2 , 4);
          }
        }
    }, true);
  }


}

module.exports = React.createClass({
  propTypes: {
    mode  : React.PropTypes.string,
    theme : React.PropTypes.string,
    name : React.PropTypes.string,
    height : React.PropTypes.string,
    width : React.PropTypes.string,
    fontSize : React.PropTypes.number,
    showGutter : React.PropTypes.bool,
    onChange: React.PropTypes.func,
    value: React.PropTypes.string,
    onLoad: React.PropTypes.func,
    maxLines : React.PropTypes.number,
    readOnly : React.PropTypes.bool,
    highlightActiveLine : React.PropTypes.bool,
    showPrintMargin : React.PropTypes.bool,
    sendBackExpression : React.PropTypes.func,
    isRepl: React.PropTypes.bool
  },
  getDefaultProps: function() {
    return {
      name   : 'brace-editor',
      mode   : 'python',
      theme  : 'monokai',
      height : '100px',
      width  : '100%',
      value  : '',
      fontSize   : 12,
      showGutter : true,
      onChange   : null,
      onLoad     : null,
      maxLines   : null,
      readOnly   : false,
      highlightActiveLine : true,
      showPrintMargin     : true,
      sendBackExpression : null,
      isRepl: false
    };
  },

  onChange: function() {
    let value = this.editor.getValue();
    if (this.props.onChange) {
      this.props.onChange(value);
    }
  },

  getRawEditor() {
    return this.editor;
  },

  handleKeyDown(e) {
    // console.log("onkeydown editor");
    if (ShortcutUtils.editorShouldDeferKey(e)) {
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    }
  },

  handleKeyUp(e) {
    if (this.props.isRepl){
      // console.log("current xp: " + JSON.stringify(this.editor.getValue()));
      let val = this.editor.getValue();
      let cursor = this.editor.selection.getCursor();
      let lastChar = val.substring(val.length-1);
      if (e.which === 13) { // pressed enter
        // KeyUtils.killEvent(e);
        if (lastChar === "\t") {
          if (cursor.column === 4){
            // console.log("padding automatic tabs");
            this.editor.getSession().indentRow(cursor.row, cursor.row, "    ");
          }
          else if (cursor.column === 1){
            val = val.substring(0,val.length-1) + "    \t";
            this.editor.setValue(val);
          }
        }
        else if (cursor.column < 4){
          // console.log("padding illegal cursor position of " + cursor.column);
          val = val.trim() + "\n    ";
          this.editor.setValue(val);
        }
        this.editor.selection.clearSelection();
      }
      else if (lastChar === "\n"){
        // console.log("padding singular newline");
        this.editor.setValue(val + "    ");
        this.editor.selection.clearSelection();
      }
    }
  },

  handleClick(e) {
    // console.log("clicked repl!");
    let cursor = this.editor.selection.getCursor();
    if (cursor.column <= 4){
      this.editor.selection.moveCursorToPosition({row: cursor.row, column: 4});
      this.editor.selection.clearSelection();
    }
  },

  componentDidMount: function() {
    this.editor = ace.edit(this.props.name);
    this.editor.$blockScrolling = Infinity;
    this.editor.on('change', this.onChange);
    this.editor.setValue(this.props.value, 1);

    onPropsSet(this.editor, this.props);
  },

  componentWillReceiveProps: function(nextProps) {
    if (this.editor.getValue() !== nextProps.value) {
      this.editor.setValue(nextProps.value, 1);
    }
    onPropsSet(this.editor, nextProps);
  },

  render: function() {
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex: 0
    };
    // console.log("ACE EDITOR HEIGHT, WIDTH: " + this.props.height + this.props.width);
    return (<div
        id={this.props.name}
        onChange={this.onChange}
        style={divStyle}
        onKeyDown={this.handleKeyDown}
        onKeyUp={this.handleKeyUp}
        onClick={this.handleClick}>
      </div>);
  }
});
