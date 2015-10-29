import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';

import Store from '../stores/ASEvaluationStore';
import ReplStore from '../stores/ASReplStore';
import FindStore from '../stores/ASFindStore';
import ExpStore from '../stores/ASExpStore';

import API from '../actions/ASApiActionCreators';
import ReplActionCreator from '../actions/ASReplActionCreators';
import ExpActionCreator from '../actions/ASExpActionCreators';

import Shortcuts from '../AS/Shortcuts';
import ShortcutUtils from '../AS/ShortcutUtils';
import ClipboardUtils from '../AS/ClipboardUtils';
import Util from '../AS/Util';
import ParseUtils from '../AS/ParsingUtils';

import Constants from '../Constants';
import TC from '../AS/TypeConversions';
import KeyUtils from '../AS/KeyUtils';
import {Snackbar} from 'material-ui';

import Repl from './repl/Repl.jsx'
import ResizableRightPanel from './repl/ResizableRightPanel.jsx'
import ASFindBar from './ASFindBar.jsx'
import ASFindModal from './ASFindModal.jsx'
import FindAction from '../actions/ASFindActionCreators';


export default React.createClass({


  /***************************************************************************************************************************/
  // React methods

  /* React method for getting the initial state */
  getInitialState() {
    return {
      language: Constants.Languages.Excel,
      varName: '',
      focus: null,
      toastMessage: '',
      toastAction: '',
      replOpen: false,
      replLanguage: Constants.Languages.Excel,
      replSubmittedLanguage: null,
      focusDx: null,
      focusDy: null,
      showFindBar:false,
      showFindModal:false
    };
  },

  /* Make sure that the evaluation pane can receive change events from the evaluation store */
  componentDidMount() {
    window.addEventListener('copy',this.handleCopyEvent);
    window.addEventListener('paste',this.handlePasteEvent);
    window.addEventListener('cut',this.handleCutEvent);
    Store.addChangeListener(this._onChange);
    FindStore.addChangeListener(this._onFindChange);
    ReplStore.addChangeListener(this._onReplChange);
    Shortcuts.addShortcuts(this);
  },

  componentWillUnmount() {
    window.removeEventListener('copy',this.handleCopyEvent);
    window.removeEventListener('paste',this.handlePasteEvent);
    window.removeEventListener('cut',this.handleCutEvent);
    API.close();
    Store.removeChangeListener(this._onChange);
    FindStore.removeChangeListener(this._onFindChange);
    ReplStore.removeChangeListener(this._onReplChange);
  },


  /***************************************************************************************************************************/
  // Component getter methods

  _getSpreadsheet() {
    return React.findDOMNode(this.refs.spreadsheet.refs.hypergrid);
  },

  _getRawEditor() {
    return this.refs.editorPane.refs.editor.getRawEditor();
  },

  _getEditorComponent(){
    return this.refs.editorPane.refs.editor;
  },

  _getDomEditor() {
    return React.findDOMNode(this.refs.editorPane.refs.editor);
  },

  _getReplEditor() {
    return this.refs.repl.refs.editor.getRawEditor();
  },

  _getTextbox(){
    return this.refs.spreadsheet.refs.textbox;
  },

  _getRawTextbox() {
    return this.refs.spreadsheet.refs.textbox.getRawEditor();
  },

  focusGrid() {
    this.refs.spreadsheet.setFocus();
  },

  /***************************************************************************************************************************/
  // Some basic on change handlers

  setLanguage(lang) {
    // TODO change dropdown when triggered programmatically
    console.log("setting language: "+JSON.stringify(lang));
    this.setState({ language: lang });
    this.refs.spreadsheet.setFocus();
  },

   _onSetVarName(name) {
    console.log('var name set to', name);
    this.setState({ varName: name });
    //TODO: set var name on backend
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
    this.refs.spreadsheet.updateCellValues(updatedCells);
    //toast the error of at least one value in the cell
    let i = 0;
    for (i = 0; i < updatedCells.length; ++i) {
      let cell = updatedCells[i],
          val = cell.cellValue;
      if (val.tag == "ValueError") {
        this.showAnyErrors(val);
        break;
      }
    }
    let extError = Store.getExternalError();
    if (extError) {
      this.setToast(extError, "ERROR");
      Store.setExternalError(null);
    }
  },

  _onReplChange() {
    console.log("Eval pane detected event change from repl store");
    this.setState({replSubmittedLanguage:ReplStore.getSubmittedLanguage()})
  },


  /**************************************************************************************************************************/
  // Error handling 

  showAnyErrors(cv) {
    this.setToast(cv.errMsg, "Error");
  },

  setToast(msg, action) {
    console.log("set toast");
    this.setState({toastMessage: msg, toastAction: action});
    this.refs.snackbarError.show();
  },

  hideToast() {
    console.log("hide toast");
    this.setState({toastMessage: "", toastAction: "hide"});
    this.refs.snackbarError.dismiss();
  },

  _handleToastTap(e) {
    // TODO
  },

  /**************************************************************************************************************************/
  /* Copy paste handling */

  handleCopyTypeEventForGrid(e,isCut) {
    // KeyUtils.killEvent(e);
    // For now, the killEvent doesn't kill fin-hypergrid's default copy handler, since
    // fin's hypergrid component is a child of ASEvaluationPane. If all this code
    // gets commented out, copy actually works mostly as expected, EXCEPT that
    // the table saved to the clipboard (from "let html = ...") doesn't have
    // id=alphasheets set, which is how we know we the clipboard content is
    // from AlphaSheets originally.
    let sel = Store.getActiveSelection(),
        vals = Store.getRowMajorCellValues(sel.range);
    if (vals) {
      Store.setClipboard(sel, isCut);
      let html = ClipboardUtils.valsToHtml(vals),
          plain = ClipboardUtils.valsToPlain(vals);
      this.refs.spreadsheet.repaint(); // render immediately
      e.clipboardData.setData("text/html",html);
      e.clipboardData.setData("text/plain",plain);
    }
  },

  handlePasteEventForGrid(e) {
    // KeyUtils.killEvent(e);
    // THIS killEvent doesn't do anything either, and that's because fin-hypergrid doesn't
    // even seem to have paste implemented by default...?
    let sel = Store.getActiveSelection(),
        containsHTML = Util.arrContains(e.clipboardData.types,"text/html"),
        containsPlain = Util.arrContains(e.clipboardData.types,"text/plain"),
        isAlphaSheets = containsHTML ?
          ClipboardUtils.htmlStringIsAlphaSheets(e.clipboardData.getData("text/html")) : false;
    if (isAlphaSheets) { // From AS
      let clipboard = Store.getClipboard(),
          fromASRange = TC.simpleToASRange(clipboard.area.range),
          toASRange = TC.simpleToASRange(sel.range);
      if (clipboard.area) {
        if (clipboard.isCut) {
          API.cut(fromASRange, toASRange);
        } else {
          API.copy(fromASRange, toASRange);
        }
      }
      else{
        this.setToast("Nothing in clipboard.", "Error");
      }
      this.refs.spreadsheet.repaint(); // render immediately
    }
    else { // Not from AS
      if (containsPlain) {
        let plain = e.clipboardData.getData("text/plain"),
            vals = ClipboardUtils.plainStringToVals(plain),
            cells = Store.makeASCellsFromPlainVals(sel,vals,this.state.language),
            concatCells = Util.concatAll(cells);
        API.pasteSimple(concatCells);
        // The normal eval handling will make the paste show up
      }
      else {
        // TODO: Not handling html conversion for now
        // Not sure if getData is smart enough to do that for you
      }
    }
  },

  /* TODO: handle other copy/paste events; from editor and textbox */

  handleCutEvent(e) {
    if (this._isEventFromGrid(e)) {
      this.handleCopyTypeEventForGrid(e,true);
    }
  },

  handleCopyEvent(e) {
    if (this._isEventFromGrid(e)) {
      this.handleCopyTypeEventForGrid(e,false);
    }
  },

  handlePasteEvent(e) {
    if (this._isEventFromGrid(e)) {
      this.handlePasteEventForGrid(e);
    }
  },

  _isEventFromGrid(e) {
    return (document.activeElement.tagName == "FIN-HYPERGRID");
  },


  /**************************************************************************************************************************/
  /* Key handling */

  _onEditorDeferredKey(e) {
    console.log("Editor deferred key");
    ShortcutUtils.tryShortcut(e, 'common');
    ShortcutUtils.tryShortcut(e, 'editor');
  },

  _onGridNavKeyDown(e) {
    console.log("Eval pane has grid's nav key");
    let insert = ExpStore.gridCanInsertRef();
    if (insert){
      // do nothing; onSelectionChange will fire
    } else {
      console.log("Will change selection and eval cell.");
      let xpObj = {
            expression: ExpStore.getExpression(),
            language: this.state.language
          };
      this.handleEvalRequest(xpObj, null, null);
    }
  },

  _onTextBoxDeferredKey(e) {
    console.log("Textbox key not visible");
    ShortcutUtils.tryShortcut(e, 'common');
    ShortcutUtils.tryShortcut(e, 'textbox');
  },

  /* Callback from Repl component */
  _onReplDeferredKey(e) {
    ShortcutUtils.tryShortcut(e, 'repl');
  },


  /**************************************************************************************************************************/
  // Deal with selection change from grid

  _onSelectionChange(sel) {
    console.log("\nEVAL PANE ON SEL CHANGE");
    console.log(this._getTextbox().editor.getCursorPosition());

    let rng = sel.range,
        userIsTyping = ExpStore.getUserIsTyping(),
        cell = Store.getCell(sel.origin.col, sel.origin.row);

    let editorCanInsertRef = ExpStore.editorCanInsertRef(this._getRawEditor()), 
        gridCanInsertRef = ExpStore.gridCanInsertRef(), 
        textBoxCanInsertRef = ExpStore.textBoxCanInsertRef(this._getTextbox().editor);

    console.log("editorCanInsertRef",editorCanInsertRef);
    console.log("gridCanInsertRef",gridCanInsertRef);
    console.log("textBoxCanInsertRef",textBoxCanInsertRef);

    let canInsertRef = editorCanInsertRef || gridCanInsertRef || textBoxCanInsertRef; 
    // Enumerate changes in selection that don't result in insertion
    let changeSelToExistingCell = cell && !userIsTyping && cell.cellExpression,
        changeSelToNewCell = !cell && !userIsTyping,
        changeSelWhileTypingNoInsert = userIsTyping && !canInsertRef;

    if (changeSelToExistingCell) {
      console.log("\n\nSelected non-empty cell to move to");
      let {language,expression} = cell.cellExpression,
          val = cell.cellValue;
      Store.setActiveSelection(sel, expression); 
      ExpActionCreator.handleSelChange(expression);
      this.showAnyErrors(val);
    } else if (changeSelToNewCell) {
      console.log("\n\nSelected empty cell to move to");
      Store.setActiveSelection(sel, "");
      this.refs.spreadsheet.repaint();
      ExpActionCreator.handleSelChange('');
      this.hideToast();
    } else if (changeSelWhileTypingNoInsert){
      console.log("Change sel while typing no insert");
      let xpObj = {
          expression: ExpStore.getExpression(),
          language: this.state.language
      };
      Store.setActiveSelection(sel, "");
      this.handleEvalRequest(xpObj, null, null);
    } else if (userIsTyping) {
      if (editorCanInsertRef){ // insert cell ref in editor
        console.log("Eval pane inserting cell ref in editor");
        let excelStr = Util.rangeToExcel(rng);
        this._getEditorComponent().insertRef(excelStr);
        let newStr = this._getRawEditor().getValue(); // new value
        ExpActionCreator.handlePartialRefEditor(newStr,excelStr);
      }
      else if (textBoxCanInsertRef){ // insert cell ref in textbox
        console.log("Eval pane inserting cell ref in textbox");
        console.log("Current value: " + this._getTextbox().editor.getValue());
        let excelStr = Util.rangeToExcel(rng);
        this._getTextbox().insertRef(excelStr);
        let newStr = this._getTextbox().editor.getValue();
        ExpActionCreator.handlePartialRefTextBox(newStr,excelStr);
      }
      else if (gridCanInsertRef){ // insert cell ref in textbox
        console.log("Eval pane inserting cell ref originating from grid");
        let excelStr = Util.rangeToExcel(rng);
        ExpActionCreator.handlePartialRefGrid(excelStr);
      }
    } else {
      console.log("\n\nUNHANDLED CASE IN ONSELECTIONCHANGE -- FIX NOW\n\n");
    }
  },

  /**************************************************************************************************************************/
  // Handle an eval request

  /*
  The editor state (langage and expression) gets here via props from the ace editor component
  Route an eval request through an API handler (eval hasn't happened yet)
    1) Get the selected region from the ASSpreadsheet component
    2) Send this and the editor state (expression, language) to the API action creator, which will send it to the backend
  */
  handleEvalRequest(xpObj, moveCol, moveRow) {
    console.log("Handling EVAL request");

    this.refs.spreadsheet.refs.textbox.hideTextBox();
    ExpStore.setLastCursorPosition(Constants.CursorPosition.GRID);
    ExpStore.setUserIsTyping(false);

    Store.setActiveCellDependencies([]);
    this.refs.spreadsheet.repaint();

    let origin = Store.getActiveSelection().origin,
        rng = {tl: origin, br: origin},
        asIndex = TC.simpleToASIndex(rng);
    if (moveCol !== null && moveRow !== null){
      this.refs.spreadsheet.shiftSelectionArea(moveCol, moveRow);
    }
    API.evaluate(asIndex, xpObj);
  },

  openSheet(sheet) {
    Store.setCurrentSheet(sheet);
    this.refs.spreadsheet.initializeBlank();
    this.refs.spreadsheet.getInitialData();
  },

  /* When a REPl request is made, first update the store and then send the request to the backend */
  handleReplRequest(xpObj) {
    ReplActionCreator.storeReplExpression(this.state.replLanguage.Display,this._replValue());
    API.evaluateRepl(xpObj);
  },

  /**************************************************************************************************************************/
  /* Focus */

  setFocus(elem) {
    if (elem === 'editor')
      this._getRawEditor().focus();
    else if (elem === 'grid')
      this.refs.spreadsheet.setFocus();
    else if (elem === 'textbox')
      this._getRawTextbox().focus();
  },

  /**************************************************************************************************************************/
  /* REPL handling methods */

  _replValue() {
    return this._getReplEditor().getValue();
  },

  /* Method for tucking in/out the REPL. */
  _toggleRepl() {
    /* Save expression in store if repl is about to close */
    if (this.state.replOpen) {
      ReplActionCreator.storeReplExpression(this.state.replLanguage.Display,this._replValue());
    } else {
      this._getReplEditor().focus();
    }
    this.setState({replOpen: !this.state.replOpen});
  },

  /*  When the REPL language changes, set state, save current text value, and set the next text value of the REPL editor */
  _onReplLanguageChange(e,index,menuItem) {
    ReplActionCreator.storeReplExpression(this.state.replLanguage.Display,this._replValue());
    let newLang = menuItem.payload;
    let newValue = ReplStore.getReplExp(newLang.Display);
    ReplStore.setLanguage(newLang);
    // console.log("REPL lang changed from " + this.state.replLanguage.Display + " to " + newLang.Display + ", new value: "+ newValue);
    this.setState({replLanguage:newLang});
  },


  /**************************************************************************************************************************/
  /* Find bar and modal */

  closeFindBar(){
    this.setState({showFindBar:false});
  },
  closeFindModal(){
    this.setState({showFindModal:false});
  },
  onFindBarEnter(){
      API.find(FindStore.getFindText());
  },
  openFindModal(){
    this.setState({showFindBar:false, showFindModal: true});
  },
  onFindBarNext(){
    FindAction.incrementSelection();
  },
  onFindBarPrev(){
    FindAction.decrementSelection();
  },
  _onFindChange(){
    this.refs.spreadsheet.repaint();
  },


  /**************************************************************************************************************************/
  // Render

  getEditorHeight() { // for future use in resize events
    return Constants.editorHeight + "px";
  },

  render() {
    let {expression, language, focus} = this.state,
        highlightFind = this.state.showFindBar || this.state.showFindModal;

    // highlightFind is for the spreadsheet to know when to highlight found locs
    // display the find bar or modal based on state
    let leftEvalPane =
      <div style={{height: '100%'}}>
        {this.state.showFindBar ? <ASFindBar onEnter={this.onFindBarEnter}
                                             onNext={this.onFindBarNext}
                                             onPrev={this.onFindBarPrev}
                                             onClose={this.closeFindBar}
                                             onModal={this.openFindModal}/> : null}
        {this.state.showFindModal ? <ASFindModal initialSelection={0}
                                                 onClose={this.closeFindModal} /> : null}
        <ASCodeEditor
          ref='editorPane'
          focusGrid={this.focusGrid}
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
          highlightFind={highlightFind}
          onNavKeyDown={this._onGridNavKeyDown}
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
  }

});
