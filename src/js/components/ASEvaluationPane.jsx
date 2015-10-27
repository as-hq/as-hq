import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import Store from '../stores/ASEvaluationStore';
import ReplStore from '../stores/ASReplStore';
import FindStore from '../stores/ASFindStore';
import API from '../actions/ASApiActionCreators';
import ReplActionCreator from '../actions/ASReplActionCreators';
import Shortcuts from '../AS/Shortcuts';
import ShortcutUtils from '../AS/ShortcutUtils';
import ClipboardUtils from '../AS/ClipboardUtils';
import Util from '../AS/Util';
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
  /* State methods */

  printDetail(type) {
    switch(type) {
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
      xpChangeOrigin:Constants.xpChange.NONE,
      expressionWithoutLastRef: '',
      language: Constants.Languages.Excel,
      varName: '',
      focus: 'grid',
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

  setLanguage(lang) {
    // TODO change dropdown when triggered programmatically
    console.log("setting language: "+JSON.stringify(lang));
    this.setState({ language: lang });
    this.refs.spreadsheet.setFocus();
  },


  /* Update the focus between the editor and the grid */
  toggleFocus() { //currently not used anywhere
    console.log("In toggle focus function");
    switch(this.state.focus) {
      case 'grid':
        this._getRawEditor().focus();
        this.setState({focus: 'editor'});
        break;
      default:
        this._getSpreadsheet().focus(); // ALEX I think you need takeFocus() ??
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
  _getReplEditor() {
    return this.refs.repl.refs.editor.getRawEditor();
  },
  focusGrid() {
    this.refs.spreadsheet.setFocus();
  },


  /**************************************************************************************************************************/

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

  shouldComponentUpdate(nextProps, nextState) {
    // Simply changing typing from false to true (upon the first grid key) shouldn't rerender
    // the whole eval pane
    if (this.state.xpChangeOrigin === Constants.xpChange.FROM_GRID){
      return false;
    }
    return true;
  },

  /**************************************************************************************************************************/

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

  _onGridDeferredKey(e) {
   if (KeyUtils.producesVisibleChar(e)) {
        let editor = this._getRawEditor(),
          str = KeyUtils.modifyStringForKey(editor.getValue(), e),
          newStr = KeyUtils.getString(e),
          xpStr = this.state.userIsTyping ? str : newStr;
      console.log("\n\nNew grid string: " + xpStr);
      this.setState({
            xpChangeOrigin:Constants.xpChange.FROM_GRID,
            expressionWithoutLastRef:xpStr,
            expression:xpStr,
          },function() {editor.navigateFileEnd();}
      );
    }
    else {
      console.log("Grid key not visible");
      ShortcutUtils.tryShortcut(e, 'common');
      ShortcutUtils.tryShortcut(e, 'grid');
    }
  },

  _onTextBoxDeferredKey(e) {
    if (KeyUtils.producesVisibleChar(e) && e.which !== 13) {
      // nothing
    } else {
      console.log("Textbox key not visible");
      ShortcutUtils.tryShortcut(e, 'common');
      ShortcutUtils.tryShortcut(e, 'textbox');
    }
  },

  _onTextBoxChange(e) {
    console.log("eval pane detected text box change");
    if (KeyUtils.producesVisibleChar(e) && e.which !== 13) {
      // nothing
    } else {
      this.setState({
        xpChangeOrigin: Constants.xpChange.FROM_TEXTBOX,
        expressionWithoutLastRef: e.target.value,
        expression: e.target.value
      });
    }
  },

  /* Callback from Repl component */
  _onReplDeferredKey(e) {
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
  _onSelectionChange(sel) {
    let rng = sel.range,
        cell = Store.getCell(sel.origin.col, sel.origin.row),
        changeSel = cell && !this.state.userIsTyping && cell.cellExpression,
        shiftSelEmpty  = this.state.userIsTyping &&
                         !Util.canInsertCellRefInXp(this.state.expressionWithoutLastRef),
        shiftSelExists = cell && shiftSelEmpty;
    if (changeSel || shiftSelExists) {
      let {language,expression} = cell.cellExpression,
          val = cell.cellValue;
      Store.setActiveSelection(sel, expression); // pass in an expression to get parsed dependencies
      // here, language is a client/server agnostic object (see Constants.Languages)
      this.setState({ xpChangeOrigin:Constants.xpChange.SEL_CHNG,
                    language: Util.getAgnosticLanguageFromServer(language),
                    expression: expression,
                    expressionWithoutLastRef:expression,
                    userIsTyping:false
                    },function() {
                      if (shiftSelExists) // selected while typing at wrong type; select away
                        this.setTextBoxVisibility(false);
                    });
      // TODO: set var name as well
      this.showAnyErrors(val);
    }
    else if (!this.state.userIsTyping || shiftSelEmpty) {
      console.log("\n\nSelected new region empty");
      Store.setActiveSelection(sel, "");
      console.log("about to set state");
      this.setState({ xpChangeOrigin:Constants.xpChange.SEL_CHNG,
                      expression: "",
                      expressionWithoutLastRef: "",
                      userIsTyping:false
                    },function() {
                      if (shiftSelEmpty) // selected while typing at wrong type; select away
                        this.setTextBoxVisibility(false);
                    });
      this.hideToast();
      console.log("after setstate call");
    }
    else {
      if (Util.canInsertCellRefInXp(this.state.expressionWithoutLastRef)) {
        console.log("PARTIAL");
        let newXp = this.state.expressionWithoutLastRef  + Util.locToExcel(rng);
        this.refs.spreadsheet._getHypergrid().repaint();
        this.setState({xpChangeOrigin:Constants.xpChange.PARTIAL_REF_CHNG,
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
  handleEvalRequest(xpObj, moveCol, moveRow) {

    /* If user pressed Ctrl Enter, they're not typing out the expression anymore */
    this.killTextbox();
    Store.setActiveCellDependencies([]);
    this.refs.spreadsheet.repaint();
    console.log("\n\nDF\n\n", Store.getActiveSelection());
    let origin = Store.getActiveSelection().origin,
        rng = {tl: origin, br: origin},
        asIndex = TC.simpleToASIndex(rng);
    this.refs.spreadsheet.shiftSelectionArea(moveCol, moveRow);
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
  /* The eval pane is the code editor plus the spreadsheet */
  getEditorHeight() { // for future use in resize events
    return Constants.editorHeight + "px";
  },

  render() {
    let {expression, language} = this.state,
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
          onTextBoxChange={this._onTextBoxChange}
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
