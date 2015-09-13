import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import Store from '../stores/ASEvaluationStore';
import ReplStore from '../stores/ASReplStore'
import API from '../actions/ASApiActionCreators';
import ReplActionCreator from '../actions/ASReplActionCreators';
import Shortcuts from '../AS/Shortcuts';
import ShortcutUtils from '../AS/ShortcutUtils';
import Util from '../AS/Util';
import Constants from '../Constants';
import Converter from '../AS/Converter'
import KeyUtils from '../AS/KeyUtils';
import {Snackbar} from 'material-ui';

import Repl from './repl/Repl.jsx'
import ResizableRightPanel from './repl/ResizableRightPanel.jsx'

export default React.createClass({


  /***************************************************************************************************************************/
  /* State methods */

  /* React method for getting the initial state */
  getInitialState() {
    return {
      expression: '',
      language: Constants.Languages.Python,
      varName: '',
      focus: 'grid',
      toastMessage: '',
      toastAction: '',
      replOpen: false,
      replLanguage: Constants.Languages.Python,
      replSubmittedLanguage: null
    };
  },
  setLanguage(lang) {
    // TODO change dropdown when triggered programmatically
    console.log("setting language: "+JSON.stringify(lang));
    this.setState({ language: lang });
  },

  setExpression(xp) {
    this.setState({ expression: xp });
  },
  /* Update the focus between the editor and the grid */
  toggleFocus() {
    console.log("In toggle focus function");
    switch(this.state.focus) {
      case 'grid':
        this._getRawEditor().focus();
        this.setState({focus: 'editor'});
        break;
      default:
        this._getSpreadsheet().focus();
        this.setState({focus: 'grid'});
        break;
    }
  },

  /***************************************************************************************************************************/
  /* Getter methods for child components of the eval pane */

  _getSpreadsheet() {
    return React.findDOMNode(this.refs.spreadsheet.refs.hypergrid);
  },
  _getRawEditor() {
    return this.refs.editorPane.refs.editor.getRawEditor();
  },
  _getDomEditor() {
    return React.findDOMNode(this.refs.editorPane.refs.editor);
  },
  _getReplEditor(){
    return this.refs.repl.refs.editor.getRawEditor();
  },
  focusGrid() {
    this._getSpreadsheet().focus();
  },

  /**************************************************************************************************************************/
  /* Make sure that the evaluation pane can receive change events from the evaluation store */

  componentDidMount() {
    Store.addChangeListener(this._onChange);
    ReplStore.addChangeListener(this._onReplChange);
    this._notificationSystem = this.refs.notificationSystem;
    Shortcuts.addShortcuts(this);
  },
  componentWillUnmount() {
    API.sendClose();
    Store.removeChangeListener(this._onChange);
    ReplStore.removeChangeListener(this._onReplChange);

  },
  addError(cv){
    if (cv.tag === "ValueError"){
      this.setToast(cv.error, "Error");
    }
  },
  setToast(msg, action) {
    this.setState({toastMessage: msg, toastAction: action});
    this.refs.snackbarError.show();
  },
  _handleToastTap(e) {
    // TODO
  },
  /*
  Upon a change event from the eval store (for example, eval has already happened)
    1) Get the last updated cells
    2) Call a ASSpreadsheet component method that forces an update of values
    3) Treat the special case of errors/other styles
  */
  _onChange() {
    console.log("Eval pane detected event change from store");
    let updatedCells = Store.getLastUpdatedCells();
    // console.log("Updated cells: " + JSON.stringify(updatedCells));
    this.refs.spreadsheet.updateCellValues(updatedCells);
    updatedCells.forEach((cell) => {
      this.addError(Converter.clientCellGetValueObj(cell));
    });
  },

  _onReplChange() {
    console.log("Eval pane detected event change from repl store");
    this.setState({replSubmittedLanguage:ReplStore.getSubmittedLanguage()})
  },

  /**************************************************************************************************************************/
  /* Key handling */

  _onEditorDeferredKey(e) {
    console.log('editor deferred key; trying common shortcut');
    console.log(e);
    ShortcutUtils.tryShortcut(e, 'common');
    ShortcutUtils.tryShortcut(e, 'editor');
  },

  _onGridDeferredKey(e) {
    if (KeyUtils.producesVisibleChar(e)) {
      console.log("key deferred by grid to editor");
      console.log(e);
      let editor = this._getRawEditor(),
          str = KeyUtils.modifyStringForKey(editor.getValue(), e);
      if (str || str === ""){
        editor.setValue(str);
        editor.navigateFileEnd();
      }
    }
    else {
      console.log('trying common shortcut');
      console.log(e);
      ShortcutUtils.tryShortcut(e, 'common');
      ShortcutUtils.tryShortcut(e, 'grid');
    }
  },

  _onReplDeferredKey(e){
    console.log('REPL deferred key');
    console.log(e);
    ShortcutUtils.tryShortcut(e, 'repl');
  },

  /**************************************************************************************************************************/
  /* Core functionality methods */

  /*
  This function is called by ASSpreadsheet on a selection change
  Deal with changing the expression in the editor when the selection in the sheet changes
    1) Get the expression at the current location clicked from the evaluation store
    2) Update the state of the evaluation pane, which forces React to rerender (and the editor to rerender)
    3) Treat the special case when the expression is an error/other styles
  */
  // Note: if the selection is a Reference, we produce a 'pseudo' expression
  // using Converter.clientCellGetExpressionObj
  _onSelectionChange(rng){
    console.log("Handling selection change: " + JSON.stringify(rng));
    let cell = Store.getCellAtLoc(rng.col,rng.row),
        {language,expression} = Converter.clientCellGetExpressionObj(cell),
        val = Converter.clientCellGetValueObj(cell);
    Store.setActiveSelection(rng, expression); // pass in an expression to get parsed dependencies
    console.log("current cell: " + JSON.stringify(cell));
    console.log("cell expression: " + expression);
    // here, language is a client/server agnostic object (see Constants.Languages)
    this.setState({ expression: expression, language: Util.getAgnosticLanguageFromServer(language) });
    // TODO: set var name as well
    this._getRawEditor().setValue(expression); // workaround for expression change bug
    this.addError(val);
  },

  /*
  The editor state (langage and expression) gets here via props from the ace editor component
  Route an eval request through an API handler (eval hasn't happened yet)
    1) Get the selected region from the ASSpreadsheet component
    2) Send this and the editor state (expression, language) to the API action creator, which will send it to the backend
  */
  handleEvalRequest(editorState){
    let selectedRegion = this.refs.spreadsheet.getSelectionArea();
    console.log("Selected region " + JSON.stringify(selectedRegion));
    console.log("Editor state: " + JSON.stringify(editorState));
    API.sendEvalRequest(selectedRegion, editorState);
  },

  /* When a REPl request is made, first update the store and then send the request to the backend */
  handleReplRequest(editorState){
    ReplActionCreator.replLeft(this.state.replLanguage.Display,this._replValue());
    console.log('handling repl request ' +  JSON.stringify(editorState));
    console.log("exps: " + JSON.stringify(ReplStore.getExps()));
    API.sendReplRequest(editorState);
  },

  /**************************************************************************************************************************/
  /* REPL handling methods */

  _replValue(){
    return this._getReplEditor().getValue();
  },

  /* Method for tucking in/out the REPL. */
  _toggleRepl(){
    /* Save expression in store if repl is about to close */
    if (this.state.replOpen){
      ReplActionCreator.replLeft(this.state.replLanguage.Display,this._replValue());
    }
    this.setState({replOpen: !this.state.replOpen});
  },

  /*  When the REPL language changes, set state, save current text value, and set the next text value of the REPL editor */
  _onReplLanguageChange(e,index,menuItem){
    ReplActionCreator.replLeft(this.state.replLanguage.Display,this._replValue());
    let newLang = menuItem.payload;
    let newValue = ReplStore.getReplExp(newLang.Display);
    console.log("REPL lang changed from " + this.state.replLanguage.Display + " to " + newLang.Display + ", new value: "+ newValue);
    this.setState({replLanguage:newLang});
  },

  /**************************************************************************************************************************/
  /* The eval pane is the code editor plus the spreadsheet */
  getEditorHeight() { // for future use in resize events
    return Constants.editorHeight + "px";
  },

  getGridHeight() {
    console.log("EVAL PANE HEIGHT: " +  this.props.height);
    let h = this.props.height - Constants.editorHeight;
    console.log("GRID HEIGHT: " + h);
    return h + "px";
  },

  render() {
    let {expression, language} = this.state;
    let leftEvalPane =
      <div >
        <ASCodeEditor
          ref='editorPane'
          language={language}
          onReplClick={this._toggleRepl}
          onLanguageChange={this.setLanguage}
          onExpressionChange={this.setExpression}
          onSetVarName={this._onSetVarName}
          onDeferredKey={this._onEditorDeferredKey}
          value={expression}
          width="100%" height={this.getEditorHeight()} />
        <ASSpreadsheet
          ref='spreadsheet'
          onDeferredKey={this._onGridDeferredKey}
          onSelectionChange={this._onSelectionChange}
          width="100%"
          height={this.getGridHeight()}  />
        <Snackbar ref="snackbarError"
                  message={this.state.toastMessage}
                  action={this.state.toastAction}
                  onActionTouchTap={this._handleToastTap} />
      </div>;

    let sidebarContent = <Repl
      ref="repl"
      replLanguage={this.state.replLanguage}
      onDeferredKey={this._onReplDeferredKey}
      onClick={this._toggleRepl}
      replValue={ReplStore.getReplExp(this.state.replLanguage.Display)}
      onReplLanguageChange={this._onReplLanguageChange} />;

    return (
      <ResizableRightPanel leftComp={leftEvalPane} sidebar={sidebarContent} docked={this.state.replOpen} />
    );
  },



  _onSetVarName(name) {
    console.log('var name set to', name);
    this.setState({ varName: name });
    //TODO: set var name on backend
  }
});
