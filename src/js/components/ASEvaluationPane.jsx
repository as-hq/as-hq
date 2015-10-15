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

  xpChange: {
    FROM_GRID:0,
    FROM_EDITOR:1,
    SEL_CHNG:2,
    PARTIAL_REF_CHNG:3,
    FROM_TEXTBOX:4,
    NONE:5
  },

  printDetail(type){
    switch(type){
      case 0:
        return "GRID";
      case 1:
        return "EDITOR";
      case 2:
        return "SEL_CHNG";
      case 3:
        return "PARTIAL_REF_CHNG";
      case 4:
        return "FROM_TEXTBOX";
      default:
        return "NONE";
    }
  },

  /* React method for getting the initial state */
  getInitialState() {
    return {
      expression: '',
      userIsTyping:false,
      xpChangeDetail:this.xpChange.NONE,
      expressionWithoutLastRef: '',
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
    // console.log("setting language: "+JSON.stringify(lang));
    this.setState({ language: lang });
  },

  updateTextBox(isTyping){
    console.log("Eval pane is updating text box, bitch");
    return this.refs.spreadsheet.refs.textbox.updateTextBox(this.state.expression,isTyping)
  },

  /* Editor has been updated, now update state in EvalPane if necessary */
  setExpression(xp) {
    console.log("New expression of type " + this.printDetail(this.state.xpChangeDetail) + ": " + xp);
    let detail = this.state.xpChangeDetail;
    let deps = Util.parseDependencies(xp);
    switch(detail){
      case this.xpChange.FROM_EDITOR:
        this.setState({expressionWithoutLastRef:xp,expression:xp,userIsTyping:true}, function(){
          this.updateTextBox(true);
        });
        console.log("User is typing");
        Store.setActiveCellDependencies(deps);
        break;
      case this.xpChange.FROM_GRID:
        if (!this.state.userIsTyping){
          this.setState({userIsTyping:true}, function(){
            this.updateTextBox(true);
          });
          console.log("User is now typing!");
        }
        else {
          this.updateTextBox(true);
        }
        Store.setActiveCellDependencies(deps);
        break;
      case this.xpChange.SEL_CHNG:
        console.log("Found selection change!");
        break;
      case this.xpChange.PARTIAL_REF_CHNG:
        console.assert(this.state.userIsTyping===true,"User should be typing");
        this.updateTextBox(true);
        Store.setActiveCellDependencies(deps);
        break;
      case this.xpChange.FROM_TEXTBOX:
        console.assert(this.state.userIsTyping===true,"User should be typing");
        this.updateTextBox(true);
        Store.setActiveCellDependencies(deps);
        break;
      default:
        Store.setActiveCellDependencies(deps);
        break;
    }
    console.log("ACTIVE: " + JSON.stringify(Store.getActiveSelection()));
    console.log("SECOND");
    this.refs.spreadsheet.repaint();
  },

  setXpDetailFromEditor(){
    this.setState({xpChangeDetail:this.xpChange.FROM_EDITOR});
  },

  textBoxChange(xp){
    console.log("Eval pane got change from text box: "+xp);
    this.setState({expression:xp,
                   expressionWithoutLastRef:xp,
                   xpChangeDetail:this.xpChange.FROM_TEXTBOX
                 });
  },

  /* Update the focus between the editor and the grid */
  toggleFocus() {
    // console.log("In toggle focus function");
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
      this.setToast(cv.errMsg, "Error");
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
    let extError = Store.getExternalError();
    if (extError) {
      this.setToast(extError, "ERROR");
      Store.setExternalError(null);
    }
  },

  _onReplChange() {
    // console.log("Eval pane detected event change from repl store");
    this.setState({replSubmittedLanguage:ReplStore.getSubmittedLanguage()})
  },

  /**************************************************************************************************************************/
  /* Key handling */

  _onEditorDeferredKey(e) {
    console.log("Editor deferred key");
    ShortcutUtils.tryShortcut(e, 'common');
    ShortcutUtils.tryShortcut(e, 'editor');
  },

  _onGridDeferredKey(e) {
    if (KeyUtils.producesVisibleChar(e)) {
      let editor = this._getRawEditor(),
          str = KeyUtils.modifyStringForKey(editor.getValue(), e),
          newStr = KeyUtils.getString(e),
          xpStr = this.state.userIsTyping ? str : newStr;
      console.log("New grid string: " + xpStr);
      this.setState({
            xpChangeDetail:this.xpChange.FROM_GRID,
            expressionWithoutLastRef:xpStr,
            expression:xpStr,
          },function(){editor.navigateFileEnd();}
      );
    }
    else {
      console.log("Grid key not visible");
      ShortcutUtils.tryShortcut(e, 'common');
      ShortcutUtils.tryShortcut(e, 'grid');
    }
  },

  _onReplDeferredKey(e){
    ShortcutUtils.tryShortcut(e, 'repl');
  },
  _onTextBoxDeferredKey(e){
    ShortcutUtils.tryShortcut(e,"common");
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
  _onSelectionChange(area){
    // console.log("Handling selection change: " + JSON.stringify(rng));
    let rng = area.range;
    let cell = Store.getCellAtLoc(rng.col,rng.row);
    let changeSel = cell && !this.state.userIsTyping && Converter.clientCellGetExpressionObj(cell),
        shiftSelEmpty  = this.state.userIsTyping && !Util.canInsertCellRefInXp(this.state.expressionWithoutLastRef),
        shiftSelExists = cell && shiftSelEmpty;
    if (changeSel || shiftSelExists) {
      let {language,expression} = Converter.clientCellGetExpressionObj(cell),
          val = Converter.clientCellGetValueObj(cell);
      Store.setActiveSelection(area, expression); // pass in an expression to get parsed dependencies
      console.log("FIRST");
      // here, language is a client/server agnostic object (see Constants.Languages)
      this.setState({ xpChangeDetail:this.xpChange.SEL_CHNG,
                    language: Util.getAgnosticLanguageFromServer(language),
                    expression: expression,
                    expressionWithoutLastRef:expression,
                    userIsTyping:false
                    },function(){
                      if (shiftSelExists) // selected while typing at wrong type; select away
                        this.updateTextBox(false);
                    });
      // TODO: set var name as well
      this.addError(val);
    }
    else if (!this.state.userIsTyping || shiftSelEmpty) {
      console.log("Selected new region empty");
      Store.setActiveSelection(area, "");
      this.setState({ xpChangeDetail:this.xpChange.SEL_CHNG,
                      expression: "",
                      expressionWithoutLastRef: "",
                      userIsTyping:false
                    },function(){
                      if (shiftSelEmpty) // selected while typing at wrong type; select away
                        this.updateTextBox(false);
                    });
    }
    else {
      if (Util.canInsertCellRefInXp(this.state.expressionWithoutLastRef)){
        console.log("PARTIAL");
        let newXp = this.state.expressionWithoutLastRef  + Util.locToExcel(rng);
        this.refs.spreadsheet._getHypergrid().repaint();
        this.setState({xpChangeDetail:this.xpChange.PARTIAL_REF_CHNG,
                       expression:newXp
        });
      }
    }
  },

  /*
  The editor state (langage and expression) gets here via props from the ace editor component
  Route an eval request through an API handler (eval hasn't happened yet)
    1) Get the selected region from the ASSpreadsheet component
    2) Send this and the editor state (expression, language) to the API action creator, which will send it to the backend
  */
  handleEvalRequest(editorState){
    /* If user pressed Ctrl Enter, they're not typing out the expression anymore */
    this.setState({userIsTyping:false});
    this.updateTextBox(false);
    Store.setActiveCellDependencies([]);
    this.refs.spreadsheet.repaint();
    this.refs.spreadsheet._getHypergrid().getSelectionModel().clear();

    let selectedRegion = Store.getActiveSelection();
    console.log("Editor state: " + JSON.stringify(editorState));
    console.log("Eval req loc: " + JSON.stringify(selectedRegion));
    API.sendEvalRequest(selectedRegion, editorState);
  },

  openSheet(sheet) {
    Store.setCurrentSheet(sheet);
    this.refs.spreadsheet.initializeBlank();
    this.refs.spreadsheet.getInitialData();
  },

  /* When a REPl request is made, first update the store and then send the request to the backend */
  handleReplRequest(editorState){
    ReplActionCreator.replLeft(this.state.replLanguage.Display,this._replValue());
    // console.log('handling repl request ' +  JSON.stringify(editorState));
    console.log("repl exps: " + JSON.stringify(ReplStore.getExps()));
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
    ReplStore.setLanguage(newLang);
    // console.log("REPL lang changed from " + this.state.replLanguage.Display + " to " + newLang.Display + ", new value: "+ newValue);
    this.setState({replLanguage:newLang});
  },

  /**************************************************************************************************************************/
  /* The eval pane is the code editor plus the spreadsheet */
  getEditorHeight() { // for future use in resize events
    return Constants.editorHeight + "px";
  },

  render() {
    let {expression, language} = this.state;
    let leftEvalPane =
      <div style={{height: '100%'}}>
        <ASCodeEditor
          ref='editorPane'
          language={language}
          onReplClick={this._toggleRepl}
          onLanguageChange={this.setLanguage}
          onExpressionChange={this.setExpression}
          setXpDetailFromEditor={this.setXpDetailFromEditor}
          onSetVarName={this._onSetVarName}
          onDeferredKey={this._onEditorDeferredKey}
          value={expression}
          width="100%" height={this.getEditorHeight()} />
        <ASSpreadsheet
          ref='spreadsheet'
          textBoxChange={this.textBoxChange}
          onDeferredKey={this._onGridDeferredKey}
          onTextBoxDeferredKey={this._onTextBoxDeferredKey}
          onSelectionChange={this._onSelectionChange}
          width="100%"
          height={`calc(100% - ${this.getEditorHeight()})`}  />
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
