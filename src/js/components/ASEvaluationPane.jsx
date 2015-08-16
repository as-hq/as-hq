import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import ASEvaluationStore from '../stores/ASEvaluationStore';
import API from '../actions/ASApiActionCreators';
import Shortcuts from '../AS/Shortcuts';
import Util from '../AS/Util';
import Constants from '../Constants';

function getEvaluationState () {
  return {
    expression: '',
    language: Constants.Languages.Python,
    focus: 'grid'
  };
}

export default React.createClass({
  _getSpreadsheet() {
    return React.findDOMNode(this.refs.spreadsheet.refs.hypergrid);
  },

  _getRawEditor() {
    return this.refs.editorPane.refs.editor.getRawEditor();
  },

  _getDomEditor() {
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
    let {expression, language} = this.state;

    return (
      <div className="full">
        <ASCodeEditor
          ref='editorPane'
          language={language}
          onLanguageChange={this.setLanguage}
          onExpressionChange={this.setExpression}
          onEvalRequest={this.handleEvalRequest}
          onDeferredKey={this._onEditorDeferredKey}
          focusGrid={this.focusGrid}
          value={expression}
          width="100%" height="100px" />
        <ASSpreadsheet
          ref='spreadsheet'
          onDeferredKey={this._onGridDeferredKey}
          onSelectionChange={this._onSelectionChange}
          width="100%"
          height="100%"  />
      </div>
    );
  },

  _onChange() {
    // this.setState(getEvaluationState());
    console.log("eval pane detected event change from store");
    let updatedCells = ASEvaluationStore.getLastUpdatedCells();
    this.refs.spreadsheet.updateCellValues(updatedCells);
    console.log(updatedCells);
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
      // console.log(e);
      let node = this._getDomEditor();
      let evt = Shortcuts.duplicateKeyDown(e.nativeEvent);
      console.log(evt);
      // TODO event dispatch has no effect
      node.dispatchEvent(evt);
    }
  },

  setLanguage(lang) {
    this.setState({ language: lang });
  },

  setExpression(xp) {
    this.setState({ expression: xp });
  },

  _onSelectionChange(rng){
    // get expression and lang of that rng from the store, and then change state to re-render editor
    console.log("handling selection change: " + JSON.stringify(rng));
    let {expression,language} = ASEvaluationStore.getExpressionAtLoc(rng);
    console.log("expression: " + expression);
    this.setState({ expression: expression, language: language });
  },

  focusGrid() {
    this._getSpreadsheet().focus();
  },

  toggleFocus() {
    switch(this.state.focus) {
      case 'grid':
        this._getEditor().focus();
        this.setState({focus: 'editor'});
        break;
      default:
        this._getSpreadsheet().focus();
        this.setState({focus: 'grid'});
        break;
    }
  },

  handleEvalRequest(editorState){
    let selectedRegion = this.refs.spreadsheet.getSelectionArea();
    console.log("eval expression: " + JSON.stringify(editorState));
    API.sendEvalRequest(selectedRegion, editorState);
  }
});
