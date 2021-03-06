import {logDebug} from '../AS/Logger';

import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';
import CellStore from '../stores/ASCellStore';
import {replEditor as zIndex} from '../styles/zIndex';
var ace = require('brace');
var React = require('react');

/* TODO: REFACTOR
Repl seems to work with enter and backspace, but I'm sure there are some edge-case bugs
Will eventually need a better way than casing on enter and backspace
Currently Ctrl Enter is always needed to submit, but this isn't the case for command line
  might be bad UX
  also our way of doing things is different from command line in general
    command line doesn't allow backspacing for example, probably parsing dynamically
*/

function onPropsSet(editor, props) {
  editor.getSession().setMode('ace/mode/'+props.mode);
  editor.setTheme('ace/theme/'+props.theme);
  editor.setFontSize(props.fontSize);
  editor.renderer.setShowGutter(props.showGutter);
  editor.setOption('maxLines', props.maxLines);
  editor.setOption('readOnly', props.readOnly);
  editor.setOption('highlightActiveLine', props.highlightActiveLine);
  editor.setShowPrintMargin(props.setShowPrintMargin);

  editor.getSession().setUseSoftTabs(false);
  editor.commands.addCommand({
    name: 'backspace',
    bindKey: {win: 'backspace',  mac: 'backspace'},
    exec(editor) {
      let pos = editor.getCursorPosition(),
          val = editor.getValue(),
          line = editor.getSession().getLine(pos.row),
          lines = editor.getValue().split('\n');
      logDebug("Old backspace value: " + JSON.stringify(lines));
      if (pos.column < 4) { // In dead-zone
        return;
      }
      if (pos.column == 4 ) { // At border
          let isAtPrompt = line.substring(0,4) === ">>> ";
          if (!isAtPrompt) {
              logDebug("Key down position: " + pos.row + " " + pos.column);
              let column = editor.getSession().getLine(pos.row-1).length;
              // goToLine starts at 1, but pos starts at 0
              if (line.trim() === "") { // line is empty
                let backspaceValPrompt = val.substring(0,val.length-5);
                editor.setValue(backspaceValPrompt);
                editor.clearSelection();
              } else { //line has content to be moved back
                lines[pos.row-1]+=lines[pos.row].substring(4);
                lines.splice(pos.row,1);
                editor.setValue(lines.join('\n'));
                editor.clearSelection();
                let prevColLength = editor.getSession().getLine(pos.row-1).length;
                editor.gotoLine(pos.row,prevColLength);
              }
              logDebug("New position: " + JSON.stringify(editor.getCursorPosition()));
              logDebug("New backspace value: " + JSON.stringify(editor.getValue().split('\n')));
              return;
            } else {
            return;
          }
      }
      lines[pos.row] = lines[pos.row].slice(0,pos.column-1) + lines[pos.row].slice(pos.column);
      let backspaceVal = lines.join('\n');
      editor.setValue(backspaceVal);
      editor.clearSelection();
      editor.gotoLine(pos.row+1,pos.column-1);
      logDebug("New backspace value: " + JSON.stringify(editor.getValue().split('\n')));
    }
  });

  if (props.onLoad) {
    props.onLoad(editor);
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
    sendBackExpression : React.PropTypes.func
  },

  getDefaultProps() {
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
      sendBackExpression : null
    };
  },

  onChange() {
    let value = this.editor.getValue();
    if (this.props.onChange) {
      this.props.onChange(value);
    }
  },

  getRawEditor() {
    return this.editor;
  },

  handleKeyDown(e) {
    /* If the repl should do something (Ctrl Enter), do so
    else don't do anything if col < 4. There's also a backspace command triggered on key down.
    Otherwise, act as usual */
    logDebug("REPL KEYDOWN: " + e.which);
    if (ShortcutUtils.replShouldDeferKey(e)) {
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    }
    let pos = this.editor.getCursorPosition();
    // If your column position on key down is less than 4 (behind >>> ), do nothing
    if (pos.column < 4 ) {
      logDebug("WTF BITCH");
      e.preventDefault();
      e.stopPropagation();
      let pos = this.editor.getCursorPosition();
      this.editor.gotoLine(pos.row+1,4);
      return;
    }
  },

  handleKeyUp(e) {
    /* Handle enter pressing in the repl */
    let val = this.editor.getValue(),
        cursor = this.editor.getCursorPosition(),
        lastChar = val.substring(val.length-1);
    if (e.which === 13) { // pressed enter
      if (lastChar === "\t") {
        if (cursor.column === 4) { // automatic tab
          this.editor.getSession().indentRow(cursor.row, cursor.row, "    ");
        } else if (cursor.column === 1) {
          val = val.substring(0,val.length-1) + "    \t";
          this.editor.setValue(val);
        }
      } else if (cursor.column < 4) { // add four spaces to the new line
        logDebug("Enter with current val: " + JSON.stringify(this.editor.getValue().split('\n')));
        let lines = this.editor.getValue().split('\n');
        lines[cursor.row]="    "+lines[cursor.row];
        this.editor.setValue(lines.join('\n'));
        this.editor.clearSelection();
        logDebug("Enter with after val: " + JSON.stringify(this.editor.getValue().split('\n')));
      }
      this.editor.selection.clearSelection();
    } // end of enter casing
    else if (lastChar === "\n") {
      this.editor.setValue(val + "    ");
      this.editor.selection.clearSelection();
    }
  },

  handleClick(e) {
    let cursor = this.editor.selection.getCursor();
    if (cursor.column <= 4) {
      this.editor.selection.moveCursorToPosition({row: cursor.row, column: 4});
      this.editor.selection.clearSelection();
    }
  },

  componentDidMount() {
    this.editor = ace.edit(this.props.name);
    this.editor.$blockScrolling = Infinity;
    this.editor.on('change', this.onChange);
    this.editor.setValue(this.props.value, 1);
    onPropsSet(this.editor, this.props);
  },

  componentWillReceiveProps(nextProps) {
    if (this.editor.getValue() !== nextProps.value) {
      this.editor.setValue(nextProps.value, 1);
    }
    onPropsSet(this.editor, nextProps);
  },

  render() {
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex,
    };
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
