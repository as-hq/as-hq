/* @flow */

import type {
  ASValue,
  ASLanguage,
  ASSheet
} from '../types/Eval';

import type {
  ASClientExpression,
  ASFocusType
} from '../types/State';

import type Textbox from './Textbox.jsx';

import {logDebug, logError, isTesting} from '../AS/Logger';

import React from 'react';
import ReactDOM from 'react-dom';
import AceEditor from './AceEditor.jsx';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import Render from '../AS/Renderers';

import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import SelectionStore from '../stores/ASSelectionStore';
// import ReplStore from '../stores/ASReplStore';
import FindStore from '../stores/ASFindStore';
import ExpStore from '../stores/ASExpStore';
import ToolbarStore from '../stores/ASToolbarStore';
import FocusStore from '../stores/ASFocusStore';

import API from '../actions/ASApiActionCreators';
import ExpActionCreator from '../actions/ASExpActionCreators';
import FocusActionCreators from '../actions/ASFocusActionCreators';
import * as CellActionCreators from '../actions/ASCellActionCreators';

import U from '../AS/Util';
import Shortcuts from '../AS/Shortcuts';

let {
  Shortcut: ShortcutUtils,
  Clipboard: ClipboardUtils,
  Parsing: ParseUtils,
  Conversion: TC,
  Key: KeyUtils
} = U;

import ASSelection from '../classes/ASSelection';

import Constants from '../Constants';
import {Snackbar} from 'material-ui';

import * as BrowserTests from '../browser-test/index';

// import Repl from './repl/Repl.jsx'
import EvalHeaderController from './eval-header/EvalHeaderController.jsx'
import ResizablePanel from './ResizablePanel.jsx'
import ASFindBar from './ASFindBar.jsx';
import ASFindModal from './ASFindModal.jsx';
import FindAction from '../actions/ASFindActionCreators';

type ASEvalPaneDefaultProps = {};

type ASEvalPaneProps = {};

type ASEvalPaneState = {
  replLanguage: ASLanguage;
  varName: string;
  toastMessage: ?string;
  toastAction: ?string;
  expression: string;
  expressionWithoutLastRef: string;
  showFindBar: boolean;
  userIsTyping: boolean;
  showFindModal: boolean;
  testMode: boolean;
  headerOpen: boolean;
};

// REPL stuff is getting temporarily phased out in favor of an Eval Header file. (Alex 11/12)
export default class ASEvalPane
  extends React.Component<ASEvalPaneDefaultProps, ASEvalPaneProps, ASEvalPaneState>
{

  _boundHandleCopyEvent: (event: SyntheticClipboardEvent) => void;
  _boundHandlePasteEvent: (event: SyntheticClipboardEvent) => void;
  _boundHandleCutEvent: (event: SyntheticClipboardEvent) => void;
  _boundOnCellsChange: () => void;
  _boundOnSheetStateChange: () => void;
  _boundOnFindChange: () => void;
  _boundOnExpChange: () => void;

  /***************************************************************************************************************************/
  // React methods

  constructor(props: ASEvalPaneProps) {
    super(props);

    this.state = {
      replLanguage: Constants.Languages.Python,
      varName: '',
      toastMessage: '',
      toastAction: '',
      expression: '',
      expressionWithoutLastRef: '',
      userIsTyping: false,
      showFindBar:false,
      showFindModal:false,
      testMode: false,
      headerOpen: false
    };
  }

  componentDidMount() {
    this._boundHandleCopyEvent = event => this.handleCopyEvent(event);
    window.addEventListener('copy', this._boundHandleCopyEvent);

    this._boundHandlePasteEvent = event => this.handlePasteEvent(event);
    window.addEventListener('paste', this._boundHandlePasteEvent);

    this._boundHandleCutEvent = event => this.handleCutEvent(event);
    window.addEventListener('cut', this._boundHandleCutEvent);

    this._boundOnCellsChange = () => this._onCellsChange();
    CellStore.addChangeListener(this._boundOnCellsChange);

    this._boundOnSheetStateChange = () => this._onSheetStateChange();
    SheetStateStore.addChangeListener(this._boundOnSheetStateChange);

    this._boundOnFindChange = () => this._onFindChange();
    FindStore.addChangeListener(this._boundOnFindChange);

    this._boundOnExpChange = () => this._onExpChange();
    ExpStore.addChangeListener(this._boundOnExpChange);

    Shortcuts.addShortcuts(this);

    FocusActionCreators.setCallbacks({
      editor: () => { this._getRawEditor().focus(); },
      grid: () => { this.getASSpreadsheet().setFocus(); },
      textbox: () => { this._getRawTextbox().focus(); },
    });

    BrowserTests.install(window, this);
  }

  /* Make sure that the evaluation pane can receive change events from the evaluation store */
  componentWillUnmount() {
    window.removeEventListener('copy', this._boundHandleCopyEvent);
    window.removeEventListener('paste', this._boundHandlePasteEvent);
    window.removeEventListener('cut', this._boundHandleCutEvent);
    API.close();
    CellStore.removeChangeListener(this._boundOnCellsChange);
    SheetStateStore.addChangeListener(this._boundOnSheetStateChange);
    FindStore.removeChangeListener(this._boundOnFindChange);
    ExpStore.removeChangeListener(this._boundOnExpChange);
  }


  /***************************************************************************************************************************/
  // Component getter methods

  _getSpreadsheet(): HGElement {
    let ele: HGElement = (ReactDOM.findDOMNode(this.getASSpreadsheet().refs.hypergrid): any);
    return ele;
  }

  _getRawEditor(): AERawClass {
    return this.refs.editorPane.refs.editor.getRawEditor();
  }

  _getEditorComponent(): AceEditor {
    return this.refs.editorPane.refs.editor;
  }

  _getDomEditor(): AEElement {
    let ele: AEElement = (ReactDOM.findDOMNode(this.refs.editorPane.refs.editor): any);
    return ele;
  }

  _getTextbox(): Textbox {
    return this.getASSpreadsheet().refs.textbox;
  }

  _getRawTextbox(): AERawClass {
    return this.getASSpreadsheet().refs.textbox.getRawEditor();
  }

  /***************************************************************************************************************************/
  // Some basic on change handlers

  // currently is never used
  _onSetVarName(name: string) {
    logDebug('var name set to', name);
    this.setState({ varName: name });
    //TODO: set var name on backend
  }

  _onSheetStateChange() {
    logDebug("Eval pane detected spreadsheet change from store");
    if (SheetStateStore.getDecoupleAttempt()) {
      let resp = true;
      // If testing, don't show confirm box. The test will send decouple msg.
      resp = window.confirm("You're attempting to decouple cells. Are you sure?");
      if (resp) {
        API.decouple();
      }
      SheetStateStore.setDecoupleAttempt(false);
    }

    // This is a terrible, terrible hack to show an error message when no cells have
    // changed, but the server returned an error. Ideally we'd create an
    // an error message store to handle this, but we're probably
    // going to do away with external errors entirely at some point, making it moot.
    // #needsrefactor
    this._onCellsChange();
  }


  _onCellsChange() {
    logDebug("Eval pane detected cells change from store");
    let updatedCellsOnSheet = CellStore.getLastUpdatedCells().filter((cell) => {
      return cell.location.sheetId == SheetStateStore.getCurrentSheet().sheetId;
    });

    this.getASSpreadsheet().updateCellValues(updatedCellsOnSheet);

    // #needsrefactor error handlers should probably get their own store
    let err = SheetStateStore.getExternalError();
    if (err != null) {
      this.setToast(err, "Error");
    }
    SheetStateStore.setExternalError(null);
  }

  // _onReplChange() {
  //   logDebug("Eval pane detected event change from repl store");
  //   this.setState({replSubmittedLanguage:ReplStore.getSubmittedLanguage()})
  // }

  _onExpChange() {
    if (ExpStore.getLastRef() === null) {
      ExpStore.disableRefInsertionBypass();
    }
    // If the language was toggled (shortcut or dropdown) then set focus correctly
    if (ExpStore.getXpChangeOrigin() === 'LANGUAGE_CHANGED'){
      FocusStore.refocus();
    }
  }

  enableTestMode() {
    this.setState({ testMode: true });
  }

  disableTestMode() {
    this.setState({ testMode: false });
  }

  getASSpreadsheet(): ASSpreadsheet {
    return this.refs.spreadsheet;
  }


  /**************************************************************************************************************************/
  // Error handling

  setToast(msg: string, action?: string) {
    // possibly truncate message
    if (msg.length > 66) {
      msg = msg.substring(0, 63) + "...";
    }
    this.setState({toastMessage: msg, toastAction: action});
    this.refs.snackbarError.show();
  }

  hideToast() {
    if (this.refs.snackbarError.state.open) {
      this.refs.snackbarError.dismiss();
    }
  }

  _handleToastTapTODO(e: SyntheticTouchEvent) {
    // TODO
    return;
  }

  /**************************************************************************************************************************/
  /* Copy paste handling */

  // cut or copy
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
    let sel = SelectionStore.getActiveSelection();
    if (! sel) {
      logDebug('No selection.'); // TODO: better handler for this. can we make it never be null
      return;
    }

    let vals = CellStore.getRowMajorCellValues(sel.range);

    logDebug('Handling copy event');

    if (vals) {
      SheetStateStore.setClipboard(sel, isCut);
      let html = ClipboardUtils.valsToHtml(vals, sel.range),
          plain = ClipboardUtils.valsToPlain(vals);
      this.getASSpreadsheet().repaint(); // render immediately
      e.clipboardData.setData("text/html",html);
      e.clipboardData.setData("text/plain",plain);
    }
  }

  handlePasteEventForGrid(e: SyntheticClipboardEvent) {
    // KeyUtils.killEvent(e);
    // THIS killEvent doesn't do anything either, and that's because fin-hypergrid doesn't
    // even seem to have paste implemented by default...?
    logDebug('Handling paste event');
    Render.setMode(null);

    let sel = SelectionStore.getActiveSelection();
    if (! sel) {
      logDebug('No selection.');
      return;
    }

    let containsHTML = e.clipboardData.types.includes("text/html"),
        containsPlain = e.clipboardData.types.includes("text/plain"),
        isAlphaSheets = this.state.testMode ||
          (
            containsHTML
              ? ClipboardUtils.htmlStringIsAlphaSheets(e.clipboardData.getData("text/html"))
              : false
          );

    // #incomplete should either be checking if you're from the same sheet, OR support
    // copy/pasting across sheets.
    if (isAlphaSheets) { // From AS
      let clipboard = SheetStateStore.getClipboard(),
          sheetId = SheetStateStore.getCurrentSheet().sheetId,
          fromRange = ClipboardUtils.getAttrsFromHtmlString(e.clipboardData.getData("text/html")),
          fromSheetId = sel.range.sheetId,
          toASRange = sel.range;

      // clipboard.area is basically obsolete, except for allowing copy/paste within the same sheets
      // for browser tests. (We need a special case for this because mocking the actual clipboard is difficult.)
      if (isTesting() || U.Browser.isMac()) {
        if (!! clipboard.area) {
          fromRange   = clipboard.area.range;
          fromSheetId = SheetStateStore.getCurrentSheetId();
        }
      }

      if (fromRange) {
        if (clipboard.isCut && sheetId == fromSheetId) { // only give cut behavior within sheets
          API.cut(fromRange, toASRange);
          SheetStateStore.setClipboard(null, false);
        } else {
          API.copy(fromRange, toASRange);
        }
      } else {
        this.setToast("Nothing in clipboard.", "Error");
      }
      this.getASSpreadsheet().repaint(); // render immediately
    } else { // Not from AS
      if (containsPlain) {
        let lang = ExpStore.getLanguage();
        let plain = e.clipboardData.getData("text/plain"),
            vals = ClipboardUtils.plainStringToVals(plain),
            evalInstructions2d = ClipboardUtils.externalStringsToEvalInstructions(sel.origin, vals, lang),
            evalInstructions = U.Array.concatAll(evalInstructions2d);
        API.pasteSimple(evalInstructions);
        // The normal eval handling will make the paste show up
      } else {
        // TODO: Not handling html conversion for now
        // Not sure if getData is smart enough to do that for you
      }
    }
  }

  /* TODO: handle other copy/paste events; from editor and textbox */

  handleCutEvent(e: SyntheticClipboardEvent) {
    if (this._isGridActive()) {
      this.handleCopyTypeEventForGrid(e, true);
    }
  }

  handleCopyEvent(e: SyntheticClipboardEvent) {
    if (this._isGridActive()) {
      this.handleCopyTypeEventForGrid(e, false);
    }
  }

  handlePasteEvent(e: SyntheticClipboardEvent) {
    if (this._isGridActive()) {
      this.handlePasteEventForGrid(e);
    }
  }

  _isGridActive(): boolean {
    return (document.activeElement.tagName == "FIN-HYPERGRID");
  }


  /**************************************************************************************************************************/
  /* Key handling */

  _onEditorDeferredKey(e: SyntheticKeyboardEvent) {
    logDebug("Editor deferred key");
    ShortcutUtils.tryEditorShortcut(e);
  }

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
            language: ExpStore.getLanguage()
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
  }

  _onTextBoxDeferredKey(e: SyntheticKeyboardEvent) {
    logDebug("Textbox key not visible");
    ShortcutUtils.tryTextboxShortcut(e);
  }

  // /* Callback from Repl component */
  // _onReplDeferredKey(e) {
  //   ShortcutUtils.tryShortcut(e, 'repl');
  // }


  /**************************************************************************************************************************/
  // Deal with selection change from grid

  _onSelectionChange(sel: ASSelection) {

    let {range, origin} = sel,
        userIsTyping = ExpStore.getUserIsTyping(),
        cell = CellStore.getCell(origin);

    let editorCanInsertRef = ExpStore.editorCanInsertRef(this._getRawEditor()),
        gridCanInsertRef = ExpStore.gridCanInsertRef(),
        textBoxCanInsertRef = ExpStore.textBoxCanInsertRef(this._getTextbox().editor);

    logDebug("Current expression: " , ExpStore.getExpression());
    logDebug("Cursor position: " , ExpStore.getLastCursorPosition());

    logDebug("Editor insert: " , editorCanInsertRef);
    logDebug("Grid insert: " , gridCanInsertRef);
    logDebug("Textbox insert: " , textBoxCanInsertRef);


    let canInsertRef = editorCanInsertRef || gridCanInsertRef || textBoxCanInsertRef;
    // Enumerate changes in selection that don't result in insertion
    let changeSelToExistingCell = cell && !userIsTyping && cell.expression,
        changeSelToNewCell = !cell && !userIsTyping,
        changeSelWhileTypingNoInsert = userIsTyping && !canInsertRef;

    if (!! cell && changeSelToExistingCell) { // !! cell for flow.
      logDebug("Selected non-empty cell to move to");
      let {language, expression} = cell.expression,
          val = cell.value;
      if (! language) {
        throw new Error('Language invalid!');
      }

      CellActionCreators.setActiveSelection(sel);
      ExpActionCreator.handleSelChange(language, expression);
      this.hideToast();

    } else if (changeSelToNewCell) {
      const language = ExpStore.getDefaultLanguage();
      CellActionCreators.setActiveSelection(sel);
      this.getASSpreadsheet().repaint();
      ExpActionCreator.handleSelChange(language, '');
      this.hideToast();

    } else if (changeSelWhileTypingNoInsert) { //click away while not parsable
      logDebug("Change sel while typing no insert");
      let xpObj = {
          expression: ExpStore.getExpression(),
          language: ExpStore.getLanguage()
      };
      // Eval needs to be called with the current activeSel;
      // Otherwise the eval result shows up in the new sel
      this.handleEvalRequest(xpObj, null, null);
      if (cell && cell.cellExpression) {
        CellActionCreators.setActiveSelection(sel);
      } else {
         CellActionCreators.setActiveSelection(sel);
         this.hideToast();
      }

    } else if (userIsTyping) {
      let excelStr = range.toExcel().toString();
      if (editorCanInsertRef) { // insert cell ref in editor
        logDebug("Eval pane inserting cell ref in editor");
        this._getEditorComponent().insertRef(excelStr);
        let newStr = this._getRawEditor().getValue(); // new value
        ExpActionCreator.handlePartialRefEditor(newStr,excelStr);
      } else if (textBoxCanInsertRef) { // insert cell ref in textbox
        logDebug("Eval pane inserting cell ref in textbox");
        logDebug("Current value: " + this._getTextbox().editor.getValue());
        this._getTextbox().insertRef(excelStr);
        let newStr = this._getTextbox().editor.getValue();
        ExpActionCreator.handlePartialRefTextBox(newStr,excelStr);
      } else if (gridCanInsertRef) { // insert cell ref in textbox
        logDebug("Eval pane inserting cell ref originating from grid");
        ExpActionCreator.handlePartialRefGrid(excelStr);
      }
    } else {
      console.assert(false);
    }
  }

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

    let selection = SelectionStore.getActiveSelection();
    if (! selection) {
      logError('No active selection');
      return;
    }

    this.getASSpreadsheet().refs.textbox.hideTextBox();
    ExpStore.setLastCursorPosition(Constants.CursorPosition.GRID);
    ExpStore.setUserIsTyping(false);

    this.getASSpreadsheet().repaint();

    let {origin} = selection;

    if (moveCol != null && moveRow != null) {
      logDebug("Shifting selection area");
      this.getASSpreadsheet().shiftAndResetSelection({
        dc: moveCol,
        dr: moveRow
      });
    }

    // Only eval if the expression is not empty, and the cell is not part of an expanding cell with
    // a different expression.
    // #incomplete this is still slightly wrong. If we're pressing ctrl+enter on the head cell (or maybe
    // any cell in the list) we might want the list to update (e.g. if it's range(a), where a is a constant
    // defined in the header. This is a detail that feels nontrivial to fix now -- the current
    // solution is probably good enough. (Alex 1/25)
    if (xpObj.expression != "") {
      const curCell = CellStore.getCell(origin);
      if (curCell == null || curCell.expandingType == null || curCell.expression.expression != xpObj.expression) {
        API.evaluate(origin, xpObj);
      }
    }

    ExpStore.setDefaultLanguage(xpObj.language);
  }

  // /* When a REPl request is made, first update the store and then send the request to the backend */
  // handleReplRequest(xpObj) {
  //   ReplActionCreator.storeReplExpression(this.state.replLanguage,this._replValue());
  //   API.evaluateRepl(xpObj);
  // }

  /**************************************************************************************************************************/
  /* Focus */

  _handleEditorFocus() { // need to remove blinking cursor from textbox
    this.getASSpreadsheet().refs.textbox.editor.renderer.$cursorLayer.hideCursor();
  }

  _getCodeEditorMaxLines(): number {
    return FocusStore.getFocus() === 'editor' ? 10 : 3;
  }

  /**************************************************************************************************************************/
  /* Header handling methods */

  toggleEvalHeader() {
    this.setState({headerOpen: !this.state.headerOpen});
  }

  _submitDebug() {
    let bugReport = window.prompt("Please describe the bug you encountered.","");
    API.bugReport(bugReport);
  }

  /**************************************************************************************************************************/
  /* Find bar and modal */

  closeFindBar() {
    this.setState({showFindBar: false});
  }
  closeFindModal() {
    this.setState({showFindModal: false});
  }
  onFindBarEnter() {
      API.find(FindStore.getFindText());
  }
  openFindModal() {
    this.setState({showFindBar: false, showFindModal: true});
  }
  onFindBarNext() {
    FindAction.incrementSelection();
  }
  onFindBarPrev() {
    FindAction.decrementSelection();
  }
  _onFindChange() {
    this.getASSpreadsheet().repaint();
  }

  /**************************************************************************************************************************/
  /* File management */

  _onFileDrop(files: Array<File>) {
    API.import(files[0]);
  }

  /**************************************************************************************************************************/
  // Render

  getEditorHeight(): string { // for future use in resize events
    return Constants.editorHeight + "px";
  }

  render() {
    const {expression, headerOpen} = this.state;
    const highlightFind = this.state.showFindBar || this.state.showFindModal;
    const currentLanguage = ExpStore.getLanguage();

    // highlightFind is for the spreadsheet to know when to highlight found locs
    // display the find bar or modal based on state
    let leftEvalPane =
      <div style={{display: 'flex', flexDirection: 'column', height: '100%'}}>
        {this.state.showFindBar &&
          <ASFindBar
            onEnter={() => this.onFindBarEnter()}
            onNext={() => this.onFindBarNext()}
            onPrev={() => this.onFindBarPrev()}
            onClose={() => this.closeFindBar()}
            onModal={() => this.openFindModal()}
          />
        }
        {this.state.showFindModal &&
          <ASFindModal
            initialSelection={0}
            onClose={() => this.closeFindModal()}
          />
        }
        <ASCodeEditor
          ref='editorPane'
          handleEditorFocus={() => this._handleEditorFocus()}
          maxLines={this._getCodeEditorMaxLines()}
          onSetVarName={name => this._onSetVarName(name)}
          onDeferredKey={event => this._onEditorDeferredKey(event)}
          value={expression}
          hideToast={() => this.hideToast()}
          // TODO(joel):
          // Change to FocusActionCreators.setFocus. Through a horrible,
          // convoluted control flow this causes a dispatch from within a
          // dispatch, since (believe it or not) this is called from within the
          // selection change dispatch.
          setFocus={FocusActionCreators.setFocus}
          width="100%" height={this.getEditorHeight()} />
        <ASSpreadsheet
          ref='spreadsheet'
          // TODO(joel):
          // Change to FocusActionCreators.setFocus. Through a horrible,
          // convoluted control flow this causes a dispatch from within a
          // dispatch, since (believe it or not) this is called from within the
          // selection change dispatch.
          setFocus={FocusActionCreators.setFocus}
          highlightFind={highlightFind}
          onNavKeyDown={event => this._onGridNavKeyDown(event)}
          onTextBoxDeferredKey={event => this._onTextBoxDeferredKey(event)}
          onSelectionChange={sel => this._onSelectionChange(sel)}
          onFileDrop={files => this._onFileDrop(files)}
          hideToast={() => this.hideToast()}
          width="100%"
          height={`calc(100% - ${this.getEditorHeight()})`}  />
        <Snackbar ref="snackbarError"
                  message={this.state.toastMessage}
                  action={this.state.toastAction}
                  onActionTouchTap={event => this._handleToastTapTODO(event)}
        />
      </div>;

    return (
      <ResizablePanel content={leftEvalPane}
                      sidebar={( <EvalHeaderController open={headerOpen} /> )}
                      sidebarVisible={headerOpen} />
    );
  }

}
