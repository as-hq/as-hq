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
      sendBackExpression : null
    };
  },

  onChange: function() {
    let value = this.editor.getValue();
    console.log("editor value changed: " + value);

    if (this.props.onChange) {
      this.props.onChange(value);
    }
  },

  getRawEditor() {
    return this.editor;
  },

  handleKeyDown(e) {
    console.log("onkeydown editor");
    if (ShortcutUtils.editorShouldDeferKey(e)) {
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    }
  },

  componentDidMount: function() {
    this.editor = ace.edit(this.props.name);
    this.editor.on('change', this.onChange);
    this.editor.setValue(this.props.value, 1);

    onPropsSet(this.editor, this.props);
  },

  componentWillReceiveProps: function(nextProps) {
    if (this.editor.getValue() !== nextProps.value) {
      this.editor.setValue(this.props.value, 1);
    }

    onPropsSet(this.editor, nextProps);
  },

  render: function() {
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex: 0
    };
    return (<div
        id={this.props.name}
        onChange={this.onChange}
        style={divStyle}
        onKeyDown={this.handleKeyDown} >
      </div>);
  }
});
