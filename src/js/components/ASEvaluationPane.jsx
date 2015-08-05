import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import ASEvaluationStore from '../stores/ASEvaluationStore';
import API from '../actions/ASApiActionCreators';
import Shortcuts from '../AS/Shortcuts';
import Util from '../AS/Util';

function getEvaluationState () {
  return {
    expression: '',
    language: 'python',
    focus: 'grid'
  };
}

export default React.createClass({
  _getSpreadsheet() {
    return React.findDOMNode(this.refs.spreadsheet.refs.hypergrid);
  },

  _getEditor() {
    return React.findDOMNode(this.refs.editorPane.refs.editor);
  },

  getInitialState() {
    return getEvaluationState();
  },

  componentDidMount() {
    ASEvaluationStore.addChangeListener(this._onChange);
  },

  componentWillUnmount() {
    ASEvaluationStore.removeChangeListener(this._onChange);
  },

  render() {
    let {language, expression} = this.state.currentCell;

    return (
      <div className="full">
        <ASCodeEditor
          ref='editorPane'
          mode={language}
          onLanguageChange={this._setLanguage}
          onExpressionChange={this._setExpression}
          onEvalRequest={this.handleEvalRequest}
          onDeferredKey={this._onEditorDeferredKey}
          focusGrid={this.focusGrid}
          value={expression}
          width="100%" height="200px" />
        <ASSpreadsheet
          ref='spreadsheet'
          onDeferredKey={this._onGridDeferredKey}
          onSelectionChange={this._onSelectionChange}
          width="100%"
          height="100%"  />
      </div>
    );
  },

  _onChange() { // assumes that the fin-hypergrid has default behavior
    console.log("eval pane detected event change from store");
    var model = this.refs.spreadsheet.getModel();
    var updatedCells = ASEvaluationStore.getLastUpdatedCells();
    var i = 0;
    for (; i < updatedCells.length; i++) // update the hypergrid values
      model.setValue(updatedCells[i].loc[1]-1, updatedCells[i].loc[0]-1, Util.showValue(updatedCells[i].value))
  },

  _onEditorDeferredKey(e) {
    if (!Shortcuts.tryCommonShortcut(e, this)) {
      console.log('unhandled keydown event!');
      console.log(e);
    }
  },

  _onGridDeferredKey(e) {
    if (!Shortcuts.tryCommonShortcut(e, this)) { // try common shortcuts, else handoff to editor
      console.log("key deferred by grid:");
      let node = this._getEditor();
      let evt = Shortcuts.duplicateKeyDown(e.nativeEvent);
      console.log(evt);
      node.dispatchEvent(evt);
    }
  },

  setLanguage(lang) {
    this.setState({ language: tmp });
  },

  setExpression(xp) {
    this.setState({ expression: xp });
  },

  _onSelectionChange(loc){
    // get expression and lang of that loc from the store, and then change state to re-render editor
    console.log("handling selection change");
    let {expression,language} = ASEvaluationStore.getExpressionAtLoc(loc);
    console.log("expression: " + expression);
    this.setState({ expression: expression, language: language });
  },

  focusGrid() {
    this._getSpreadsheet().focus();
  },

  toggleFocus() {
    if (this.state.focus === 'grid') {
      this._getEditor().focus();
      this.setState({focus: 'editor'});
    } else {
      this._getSpreadsheet().focus();
      this.setState({focus: 'grid'});
    }
  },

  handleEvalRequest(editorState){
    console.log("eval requested!");
    let selectedRegion = this.refs.spreadsheet.getSelectionArea();
    console.log("topleft: " + selectedRegion.topLeft);
    console.log("eval expression: " + editorState);
    API.sendEvalRequest(selectedRegion, editorState);
  }
});
