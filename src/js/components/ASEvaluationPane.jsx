/* @flow */

import type {
  ASValue,
  ASLanguage,
  ASSheet
} from '../types/Eval';

import type {
  ASSelection,
  ASClientExpression,
  ASFocusType
} from '../types/State';

import type Textbox from './Textbox.jsx';

import {logDebug, logError, isTesting} from '../AS/Logger';

import React from 'react';
import ReactDOM from 'react-dom';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import Render from '../AS/Render';

import Store from '../stores/ASEvaluationStore';
// import ReplStore from '../stores/ASReplStore';
import EvalHeaderStore from '../stores/ASEvalHeaderStore';
import FindStore from '../stores/ASFindStore';
import ExpStore from '../stores/ASExpStore';

import API from '../actions/ASApiActionCreators';
// import ReplActionCreator from '../actions/ASReplActionCreators';
import EvalHeaderActionCreator from '../actions/ASEvalHeaderActionCreators';
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

import * as BrowserTests from '../browser-test/index';

// import Repl from './repl/Repl.jsx'
import EvalHeader from './eval-header/EvalHeader.jsx'
import ResizableRightPanel from './repl/ResizableRightPanel.jsx'
import ASFindBar from './ASFindBar.jsx'
import ASFindModal from './ASFindModal.jsx'
import FindAction from '../actions/ASFindActionCreators';

type ASEvalPaneState = {
  defaultLanguage: ASLanguage;
  currentLanguage: ASLanguage;
  varName: string;
  focus: ?ASFocusType;
  toastMessage: ?string;
  toastAction: ?string;
  expression: string;
  expressionWithoutLastRef: string;
  evalHeaderOpen: boolean;
  evalHeaderLanguage: ASLanguage;
  showFindBar: boolean;
  showFindModal: boolean;
  testMode: boolean;
};


// REPL stuff is getting temporarily phased out in favor of an Eval Header file. (Alex 11/12)

export default React.createClass({


  /***************************************************************************************************************************/
  // React methods

  /* React method for getting the initial state */
  getInitialState(): ASEvalPaneState {
    return {
      defaultLanguage: Constants.Languages.Excel, // the language displayed on a blank cell
      currentLanguage: Constants.Languages.Excel, // the language currently displayed
      varName: '',
      focus: null,
      toastMessage: '',
      toastAction: '',
      expression: '',
      expressionWithoutLastRef: '',
      // replOpen: false,
      // replLanguage: Constants.Languages.Python,
      // replSubmittedLanguage: null,
      evalHeaderOpen: false,
      evalHeaderLanguage: Constants.Languages.Python,
      showFindBar:false,
      showFindModal:false,
      testMode: false
    };
  },

  componentDidMount() {
    window.addEventListener('copy',this.handleCopyEvent);
    window.addEventListener('paste',this.handlePasteEvent);
    window.addEventListener('cut',this.handleCutEvent);
    Store.addChangeListener(this._onChange);
    FindStore.addChangeListener(this._onFindChange);
    // ReplStore.addChangeListener(this._onReplChange);
    EvalHeaderStore.addChangeListener(this._onEvalHeaderUpdate);
    ExpStore.addChangeListener(this._onExpChange);
    Shortcuts.addShortcuts(this);

    BrowserTests.install(window, this);
  },

  /* Make sure that the evaluation pane can receive change events from the evaluation store */
  componentWillUnmount() {
    window.removeEventListener('copy',this.handleCopyEvent);
    window.removeEventListener('paste',this.handlePasteEvent);
    window.removeEventListener('cut',this.handleCutEvent);
    API.close();
    Store.removeChangeListener(this._onChange);
    FindStore.removeChangeListener(this._onFindChange);
    // ReplStore.removeChangeListener(this._onReplChange);
    ExpStore.removeChangeListener(this._onExpChange);
  },


  /***************************************************************************************************************************/
  // Component getter methods

  _getSpreadsheet(): HGElement {
    let ele: HGElement = (ReactDOM.findDOMNode(this.refs.spreadsheet.refs.hypergrid): any);
    return ele;
  },

  _getRawEditor(): AERawClass {
    return this.refs.editorPane.refs.editor.getRawEditor();
  },

  _getEditorComponent(): ASCodeEditor {
    return this.refs.editorPane.refs.editor;
  },

  _getDomEditor(): AEElement {
    let ele: AEElement = (ReactDOM.findDOMNode(this.refs.editorPane.refs.editor): any);
    return ele;
  },

  // _getReplEditor() {
  //   return this.refs.repl.refs.editor.getRawEditor();
  // },

  _getEvalHeaderEditor(): AERawClass {
    return this.refs.evalHeader.refs.editor.getRawEditor();
  },

  _getTextbox(): Textbox {
    return this.refs.spreadsheet.refs.textbox;
  },

  _getRawTextbox(): AERawClass {
    return this.refs.spreadsheet.refs.textbox.getRawEditor();
  },

  /***************************************************************************************************************************/
  // Some basic on change handlers

  selectLanguage(lang: ASLanguage) {
    ExpStore.setLanguage(lang);
    this.setState({ defaultLanguage: lang, currentLanguage: lang });
    this.setFocus(Store.getFocus());
  },

  _onSetVarName(name: string) {
    logDebug('var name set to', name);
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
    if (Store.getDecoupleAttempt()) {
      let resp = true;
      // If testing, don't show confirm box. The test will send decouple msg.
      if (!isTesting()) {
        resp = window.confirm("You're attempting to decouple cells. Are you sure?");
        if (resp) {
          API.decouple();
        }
      }
      Store.setDecoupleAttempt(false);
    }
    logDebug("Eval pane detected event change from store");
    let updatedCellsOnSheet = Store.getLastUpdatedCells().filter((cell) => {
      return cell.cellLocation.sheetId == Store.getCurrentSheet().sheetId;
    });
    this.refs.spreadsheet.updateCellValues(updatedCellsOnSheet);
    //toast the error of at least one value in the cell
    let err;
    for (let i = 0; i < updatedCellsOnSheet.length; ++i) {
      let cell = updatedCellsOnSheet[i],
          val = cell.cellValue;
      if (val.tag == "ValueError" || val.tag == "ValueExcelError") {
        err = this.getErrorMessage(val);
        break;
      }
    }

    err = err || Store.getExternalError();

    if (!!err && !Store.shouldSuppressErrors()) {
      this.setToast(err, "Error");
    }

    Store.setExternalError(null);
    Store.stopSuppressingErrors();
  },

  // _onReplChange() {
  //   logDebug("Eval pane detected event change from repl store");
  //   this.setState({replSubmittedLanguage:ReplStore.getSubmittedLanguage()})
  // },

  _onExpChange() {
    if (ExpStore.getLastRef() === null) {
      ExpStore.disableRefInsertionBypass();
    }
  },

  _onEvalHeaderUpdate() {
    let msg = EvalHeaderStore.getDispMessage();
    if (!! msg && msg !== "") {
      this.setToast(msg);
    }
    this._getEvalHeaderEditor().focus();
  },

  enableTestMode() {
    this.setState({ testMode: true });
  },

  disableTestMode() {
    this.setState({ testMode: false });
  },


  /**************************************************************************************************************************/
  // Error handling

  getErrorMessage(cv: ASValue): ?string {
    if (cv.tag === "ValueError") {
      return cv.errorMsg;
    } else if (cv.tag === "ValueExcelError") {
      return cv.contents.tag; // ValueExcelError should become a part of ValueError eventually
    }
    return null;
  },

  showAnyErrors(cv: ASValue) {
    let err = this.getErrorMessage(cv);
    if (err) {
      this.setToast(err, "Error");
    }
  },

  setToast(msg: string, action?: string) {
    // possibly truncate message
    if (msg.length > 66) {
      msg = msg.substring(0, 63) + "...";
    }
    this.setState({toastMessage: msg, toastAction: action});
    this.refs.snackbarError.show();
  },

  // I think this is equivalent to the toast being hidden, but not sure -- RITESH
  toastIsHidden() : boolean {
    let {toastMessage, toastAction} = this.state;
    return (toastMessage == "") && (toastAction === "hide");
  },

  hideToast() {
    // Only hide a toast (and cause a rerender) if toast isn't already hidden
    if (!this.toastIsHidden()) {
      this.setState({toastMessage: "", toastAction: "hide"});
    }
    this.refs.snackbarError.dismiss();
  },

  _handleToastTap(e: SyntheticTouchEvent) {
    // TODO
    return;
  },

  /**************************************************************************************************************************/
  /* Copy paste handling */

  handleCopyTypeEventForGrid(e: SyntheticClipboardEvent, isCut?: boolean) {
    KeyUtils.killEvent(e);
    // For now, the killEvent doesn't kill fin-hypergrid's default copy handler, since
    // fin's hypergrid component is a child of ASEvaluationPane. If all this code
    // gets commented out, copy actually works mostly as expected, EXCEPT that
    // the table saved to the clipboard (from "let html = ...") doesn't have
    // id=alphasheets set, which is how we know we the clipboard content is
    // from AlphaSheets originally.
    //
    // Alex 10/29 -- nope, killEvent actually does something, and I don't understand what.
    // I DO know that if you leave it out, cut doesn't save anything to the clipboard
    // if there's already external data on the clipboard, but copy DOES work, and I don't
    // understand why.
    let sel = Store.getActiveSelection();
    if (! sel) {
      logDebug('No selection.'); // TODO: better handler for this. can we make it never be null
      return;
    }

    let vals = Store.getRowMajorCellValues(sel.range);

    logDebug('Handling copy event');

    if (vals) {
      Store.setClipboard(sel, isCut);
      let html = ClipboardUtils.valsToHtml(vals, sel.range),
          plain = ClipboardUtils.valsToPlain(vals);
      this.refs.spreadsheet.repaint(); // render immediately
      e.clipboardData.setData("text/html",html);
      e.clipboardData.setData("text/plain",plain);
    }
  },

  handlePasteEventForGrid(e: SyntheticClipboardEvent) {
    // KeyUtils.killEvent(e);
    // THIS killEvent doesn't do anything either, and that's because fin-hypergrid doesn't
    // even seem to have paste implemented by default...?
    logDebug('Handling paste event');
    Render.setMode(null);

    let sel = Store.getActiveSelection();
    if (! sel) {
      logDebug('No selection.');
      return;
    }

    let containsHTML = Util.arrContains(e.clipboardData.types,"text/html"),
        containsPlain = Util.arrContains(e.clipboardData.types,"text/plain"),
        isAlphaSheets = this.state.testMode ||
          (
            containsHTML
              ? ClipboardUtils.htmlStringIsAlphaSheets(e.clipboardData.getData("text/html"))
              : false
          );

    // #incomplete should either be checking if you're from the same sheet, OR support
    // copy/pasting across sheets.
    if (isAlphaSheets) { // From AS
      let clipboard = Store.getClipboard(),
          sheetId = Store.getCurrentSheet().sheetId,
          {fromSheetId, fromRange} = ClipboardUtils.getAttrsFromHtmlString(e.clipboardData.getData("text/html")),
          toASRange = TC.simpleToASRange(sel.range);

      // clipboard.area is basically obsolete, except for allowing copy/paste within the same sheets
      // for browser tests. (We need a special case for this because mocking the actual clipboard is difficult.)
      if (isTesting() || Util.isMac()) {
        if (!! clipboard.area) {
          fromRange   = clipboard.area.range;
          fromSheetId = Store.getCurrentSheet().sheetId;
        }
      }

      let fromASRange = TC.simpleToASRange(fromRange, fromSheetId);
      if (fromRange) {
        if (clipboard.isCut && sheetId == fromSheetId) { // only give cut behavior within sheets
          API.cut(fromASRange, toASRange);
          Store.setClipboard(null, false);
        } else {
          API.copy(fromASRange, toASRange);
        }
      } else {
        this.setToast("Nothing in clipboard.", "Error");
      }
      this.refs.spreadsheet.repaint(); // render immediately
    } else { // Not from AS
      if (containsPlain) {
        let plain = e.clipboardData.getData("text/plain"),
            vals = ClipboardUtils.plainStringToVals(plain),
            cells = ClipboardUtils.externalStringsToASCells(sel.origin, vals, this.state.currentLanguage),
            concatCells = Util.concatAll(cells);
        API.pasteSimple(concatCells);
        // The normal eval handling will make the paste show up
      } else {
        // TODO: Not handling html conversion for now
        // Not sure if getData is smart enough to do that for you
      }
    }
  },

  /* TODO: handle other copy/paste events; from editor and textbox */

  handleCutEvent(e: SyntheticClipboardEvent) {
    if (this._isGridActive()) {
      this.handleCopyTypeEventForGrid(e,true);
    }
  },

  handleCopyEvent(e: SyntheticClipboardEvent) {
    if (this._isGridActive()) {
      this.handleCopyTypeEventForGrid(e,false);
    }
  },

  handlePasteEvent(e: SyntheticClipboardEvent) {
    if (this._isGridActive()) {
      this.handlePasteEventForGrid(e);
    }
  },

  _isGridActive(): boolean {
    return (document.activeElement.tagName == "FIN-HYPERGRID");
  },


  /**************************************************************************************************************************/
  /* Key handling */

  _onEditorDeferredKey(e: SyntheticKeyboardEvent) {
    logDebug("Editor deferred key");
    ShortcutUtils.tryShortcut(e, 'common');
    ShortcutUtils.tryShortcut(e, 'editor');
  },

  _onGridNavKeyDown(e: SyntheticKeyboardEvent) {
    // should only get called if left, right, down, or up was pressed
    logDebug("Eval pane has grid's nav key");
    let canInsert = ExpStore.gridCanInsertRef();
    if (canInsert) {
      // do nothing; onSelectionChange will fire
    } else if (ExpStore.getUserIsTyping()) {
      logDebug("Will change selection and eval cell.");
      let xpObj = {
            expression: ExpStore.getExpression(),
            language: this.state.currentLanguage
          };
      // Hypergrid automatically changes the selection when you arrive here through
      // left, right, down, or up.
      this.handleEvalRequest(xpObj, 0, 0);
    } else {
      // NOTE: the "Jump to A1" bug happens if the code gets here
      // Hypothesis: hypergrid's selections are empty, and a nav key defaults to A1
      // This used to happen when double click put focus on grid (it seems that double click = no selections for hypergrid)
      // and then you press a nav key
      // Shouldn't happen anymore -- RITESH
    }

  },

  _onGridDeferredKey(e: SyntheticKeyboardEvent) {
    logDebug('\n\n\nGRID DEFERRED KEY\n\n\n', e);
    if (KeyUtils.producesTextChange(e)) {
      let editor = this._getRawEditor(),
          str = KeyUtils.appendStringByKey(editor.getValue(), e),
          newStr = KeyUtils.keyToString(e),
          xpStr = this.state.userIsTyping ? str : newStr;
      logDebug("New grid string: " + xpStr);
      this.setState({
        expressionWithoutLastRef: xpStr,
        expression: xpStr,
      }, () => { editor.navigateFileEnd(); });
    } else {
      logDebug("Grid key not visible");
      ShortcutUtils.tryShortcut(e, 'common');
      ShortcutUtils.tryShortcut(e, 'grid');
    }
  },

  _onTextBoxDeferredKey(e: SyntheticKeyboardEvent) {
    logDebug("Textbox key not visible");
    ShortcutUtils.tryShortcut(e, 'common');
    ShortcutUtils.tryShortcut(e, 'textbox');
  },

  // /* Callback from Repl component */
  // _onReplDeferredKey(e) {
  //   ShortcutUtils.tryShortcut(e, 'repl');
  // },


  /**************************************************************************************************************************/
  // Deal with selection change from grid

  _onSelectionChange(sel: ASSelection) {
    let {range, origin} = sel,
        userIsTyping = ExpStore.getUserIsTyping(),
        cell = Store.getCell(origin.col, origin.row);

    let editorCanInsertRef = ExpStore.editorCanInsertRef(this._getRawEditor()),
        gridCanInsertRef = ExpStore.gridCanInsertRef(),
        textBoxCanInsertRef = ExpStore.textBoxCanInsertRef(this._getTextbox().editor);

    logDebug("Current expression: " + ExpStore.getExpression());
    logDebug("Cursor position: " + ExpStore.getLastCursorPosition());

    logDebug("Editor insert: " + editorCanInsertRef);
    logDebug("Grid insert: " + gridCanInsertRef);
    logDebug("Textbox insert: " + textBoxCanInsertRef);


    let canInsertRef = editorCanInsertRef || gridCanInsertRef || textBoxCanInsertRef;
    // Enumerate changes in selection that don't result in insertion
    let changeSelToExistingCell = cell && !userIsTyping && cell.cellExpression,
        changeSelToNewCell = !cell && !userIsTyping,
        changeSelWhileTypingNoInsert = userIsTyping && !canInsertRef;

    if (!! cell && changeSelToExistingCell) { // !! cell for flow.
      logDebug("Selected non-empty cell to move to");
      let {language, expression} = cell.cellExpression,
          val = cell.cellValue;
      if (! language) {
        throw new Error('Language invalid!');
      }

      Store.setActiveSelection(sel, expression, language);
      ExpActionCreator.handleSelChange(expression);
      this.hideToast();
      this.showAnyErrors(val);
      if (this.state.currentLanguage !== language) {
        this.setState({currentLanguage: language});
      }
      ExpStore.setLanguage(language);
    } else if (changeSelToNewCell) {
      logDebug("Selected empty cell to move to");
      Store.setActiveSelection(sel, "", null);
      this.refs.spreadsheet.repaint();
      ExpActionCreator.handleSelChange('');
      if (this.state.currentLanguage !== this.state.defaultLanguage) {
        this.setState({currentLanguage: this.state.defaultLanguage});
      }
      ExpStore.setLanguage(this.state.defaultLanguage);
      this.hideToast();
    } else if (changeSelWhileTypingNoInsert) { //click away while not parsable
      logDebug("Change sel while typing no insert");
      let xpObj = {
          expression: ExpStore.getExpression(),
          language: this.state.currentLanguage
      };
      // Eval needs to be called with the current activeSel;
      // Otherwise the eval result shows up in the new sel
      this.handleEvalRequest(xpObj, null, null);
      if (cell && cell.cellExpression) {
        let {expression, language} = cell.cellExpression;
        Store.setActiveSelection(sel, expression, language);
        this.showAnyErrors(cell.cellValue);
      } else {
         Store.setActiveSelection(sel, "", null);
         this.hideToast();
      }
    } else if (userIsTyping) {
      if (editorCanInsertRef) { // insert cell ref in editor
        logDebug("Eval pane inserting cell ref in editor");
        let excelStr = Util.rangeToExcel(range);
        this._getEditorComponent().insertRef(excelStr);
        let newStr = this._getRawEditor().getValue(); // new value
        ExpActionCreator.handlePartialRefEditor(newStr,excelStr);
      } else if (textBoxCanInsertRef) { // insert cell ref in textbox
        logDebug("Eval pane inserting cell ref in textbox");
        logDebug("Current value: " + this._getTextbox().editor.getValue());
        let excelStr = Util.rangeToExcel(range);
        this._getTextbox().insertRef(excelStr);
        let newStr = this._getTextbox().editor.getValue();
        ExpActionCreator.handlePartialRefTextBox(newStr,excelStr);
      } else if (gridCanInsertRef) { // insert cell ref in textbox
        logDebug("Eval pane inserting cell ref originating from grid");
        let excelStr = Util.rangeToExcel(range);
        ExpActionCreator.handlePartialRefGrid(excelStr);
      }
    } else {
      console.assert(false);
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
  handleEvalRequest(xpObj: ASClientExpression, moveCol: ?number, moveRow: ?number) {
    logDebug("Handling EVAL request " + ExpStore.getExpression());

    let selection = Store.getActiveSelection();
    if (! selection) {
      logError('No active selection');
      return;
    }

    this.refs.spreadsheet.refs.textbox.hideTextBox();
    ExpStore.setLastCursorPosition(Constants.CursorPosition.GRID);
    ExpStore.setUserIsTyping(false);

    Store.setActiveCellDependencies([]);
    this.refs.spreadsheet.repaint();

    let {origin} = selection;

    if (moveCol !== null && moveRow !== null) {
      logDebug("Shifting selection area");
      this.refs.spreadsheet.shiftSelectionArea(moveCol, moveRow);
    }

    // Only re-eval if the cell actually changed from before.
    let curCell = Store.getCell(origin.col, origin.row);
    if (!curCell) {
      if (xpObj.expression != "") {
        API.evaluate(origin, xpObj);
      }
    } else {
      let {expression, language} = curCell.cellExpression;
      if (expression != xpObj.expression || language != xpObj.language) {
        API.evaluate(origin, xpObj);
      }
    }
    this.setState({defaultLanguage: xpObj.language});
  },

  openSheet(sheet: ASSheet) {
    Store.setCurrentSheet(sheet);
    this.refs.spreadsheet.initializeBlank();
    this.refs.spreadsheet.getInitialData();
  },

  // /* When a REPl request is made, first update the store and then send the request to the backend */
  // handleReplRequest(xpObj) {
  //   ReplActionCreator.storeReplExpression(this.state.replLanguage,this._replValue());
  //   API.evaluateRepl(xpObj);
  // },

  /**************************************************************************************************************************/
  /* Focus */

  setFocus(elem) {
    if (elem === 'editor') {
      this._getRawEditor().focus();
    } else if (elem === 'grid') {
      this.refs.spreadsheet.setFocus();
    } else if (elem === 'textbox') {
      this._getRawTextbox().focus();
    }
  },

  _handleEditorFocus() { // need to remove blinking cursor from textbox
    this.refs.spreadsheet.refs.textbox.editor.renderer.$cursorLayer.hideCursor();
  },

  _getCodeEditorMaxLines(): number {
    return (Store.getFocus() == 'editor') ? 10 : 3;
  },

  /**************************************************************************************************************************/
  /* REPL handling methods */

  // _replValue() {
  //   return this._getReplEditor().getValue();
  // },

  _evalHeaderValue(): string {
    return this._getEvalHeaderEditor().getValue();
  },

  _toggleRepl() {
    logDebug('Not implemented.'); // TODO
  },
  // /* Method for tucking in/out the REPL. */
  // _toggleRepl() {
  //   /* Save expression in store if repl is about to close */
  //   if (this.state.replOpen) {
  //     ReplActionCreator.storeReplExpression(this.state.replLanguage,this._replValue());
  //   } else {
  //     this._getReplEditor().focus();
  //   }
  //   this.setState({replOpen: !this.state.replOpen});
  // },

  toggleEvalHeader() {
    /* Save expression in store if repl is about to close */
    if (this.state.evalHeaderOpen) {
      // might be redundant? (Alex 11/24)
      EvalHeaderActionCreator.storeEvalHeaderExpression(this.state.evalHeaderLanguage,
                                                        this._evalHeaderValue());
    } else {
      this._getEvalHeaderEditor().focus();
    }
    this.setState({evalHeaderOpen: !this.state.evalHeaderOpen});
  },

  _submitDebug() {
    let bugReport = window.prompt("Please describe the bug you encountered.","");
    API.bugReport(bugReport);
  },

  // /*  When the REPL language changes, set state, save current text value, and set the next text value of the REPL editor */
  // _onReplLanguageChange(e,index,menuItem) {
  //   ReplActionCreator.storeReplExpression(this.state.replLanguage,this._replValue());
  //   let newLang = menuItem.payload;
  //   let newValue = ReplStore.getReplExp(newLang);
  //   ReplStore.setLanguage(newLang);
  //   // logDebug("REPL lang changed from " + this.state.replLanguage + " to " + newLang + ", new value: "+ newValue);
  //   this.setState({replLanguage:newLang});
  // },

  _onEvalHeaderLanguageChange(e: {}, index: number, menuItem: { payload: ASLanguage; }) {
    // If e is ever actually used, we will notice and remove the {} annotation.
    EvalHeaderActionCreator.storeEvalHeaderExpression(this.state.evalHeaderLanguage,
                                                      this._evalHeaderValue());
    let newLang = menuItem.payload;
    let newValue = EvalHeaderStore.getEvalHeaderExp(newLang);
    EvalHeaderStore.setLanguage(newLang);
    this.setState({evalHeaderLanguage: newLang});
  },

  _onSubmitEvalHeader() {
    let lang       = this.state.evalHeaderLanguage,
        expression = this._evalHeaderValue();

    API.evaluateHeader(expression, lang);
  },

  /**************************************************************************************************************************/
  /* Find bar and modal */

  closeFindBar() {
    this.setState({showFindBar:false});
  },
  closeFindModal() {
    this.setState({showFindModal:false});
  },
  onFindBarEnter() {
      API.find(FindStore.getFindText());
  },
  openFindModal() {
    this.setState({showFindBar:false, showFindModal: true});
  },
  onFindBarNext() {
    FindAction.incrementSelection();
  },
  onFindBarPrev() {
    FindAction.decrementSelection();
  },
  _onFindChange() {
    this.refs.spreadsheet.repaint();
  },

  /**************************************************************************************************************************/
  /* File management */

  _exportFile() { API.export(Store.getCurrentSheet()); },

  _onFileDrop(files: Array<File>) { API.import(files[0]); },

  /**************************************************************************************************************************/
  // Render

  getEditorHeight(): string { // for future use in resize events
    return Constants.editorHeight + "px";
  },

  render() {
    let {expression, currentLanguage, focus} = this.state,
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
          handleEditorFocus={this._handleEditorFocus}
          getCodeEditorMaxLines={this._getCodeEditorMaxLines}
          language={currentLanguage}
          onReplClick={this._toggleRepl}
          onExport={this._exportFile}
          onEvalHeaderClick={this.toggleEvalHeader}
          onSubmitDebug={this._submitDebug}
          onSelectLanguage={this.selectLanguage}
          onSetVarName={this._onSetVarName}
          onDeferredKey={this._onEditorDeferredKey}
          value={expression}
          hideToast={this.hideToast}
          width="100%" height={this.getEditorHeight()} />
        <ASSpreadsheet
          ref='spreadsheet'
          language={currentLanguage}
          setFocus={this.setFocus}
          highlightFind={highlightFind}
          onNavKeyDown={this._onGridNavKeyDown}
          onTextBoxDeferredKey={this._onTextBoxDeferredKey}
          onSelectionChange={this._onSelectionChange}
          onFileDrop={this._onFileDrop}
          hideToast={this.hideToast}
          width="100%"
          height={`calc(100% - ${this.getEditorHeight()})`}  />
        <Snackbar ref="snackbarError"
                  message={this.state.toastMessage}
                  action={this.state.toastAction}
                  onActionTouchTap={this._handleToastTap} />
      </div>;

    // let sidebarContent = <Repl
    //   ref="repl"
    //   replLanguage={this.state.replLanguage}
    //   onDeferredKey={this._onReplDeferredKey}
    //   onClick={this._toggleRepl}
    //   replValue={ReplStore.getReplExp(this.state.replLanguage)}
    //   onReplLanguageChange={this._onReplLanguageChange} />;

    let sidebarContent = <EvalHeader
      ref="evalHeader"
      evalHeaderLanguage={this.state.evalHeaderLanguage}
      evalHeaderValue={EvalHeaderStore.getEvalHeaderExp(this.state.evalHeaderLanguage)}
      onEvalHeaderLanguageChange={this._onEvalHeaderLanguageChange}
      onSubmitEvalHeader={this._onSubmitEvalHeader} />;

    return (
      <ResizableRightPanel leftComp={leftEvalPane} sidebar={sidebarContent} docked={this.state.evalHeaderOpen} />
    );
  }

});
