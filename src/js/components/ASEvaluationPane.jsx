import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import Store from '../stores/ASEvaluationStore';
import API from '../actions/ASApiActionCreators';
import Shortcuts from '../AS/Shortcuts';
import ShortcutUtils from '../AS/ShortcutUtils';
import Util from '../AS/Util';
import Constants from '../Constants';
import Converter from '../AS/Converter'
import KeyUtils from '../AS/KeyUtils';
import {Snackbar} from 'material-ui';

export default React.createClass({


  /***************************************************************************************************************************/
  /* State methods */

  /* React method for getting the initial state */
  getInitialState() {
    return {
      expression: '',
      language: Constants.Languages.Python,
      focus: 'grid',
      toastErrorMessasge: ''
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
  focusGrid() {
    this._getSpreadsheet().focus();
  },

  /**************************************************************************************************************************/
  /* Make sure that the evaluation pane can receive change events from the evaluation store */

  componentDidMount() {
    Store.addChangeListener(this._onChange);
    this._notificationSystem = this.refs.notificationSystem;
    Shortcuts.addShortcuts(this);
  },
  componentWillUnmount() {
    Store.removeChangeListener(this._onChange);
  },
  addError(cv){
    if (cv.tag === "ValueError"){
      this.setState({toastErrorMessasge: cv.error});
      this.refs.snackbarError.show();
    }
  },
  _handleErrorTap(e) {
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
    for (var key in updatedCells){
      let cv = Converter.clientCellGetValueObj(updatedCells[key]);
      this.addError(cv);
    }
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

  /**************************************************************************************************************************/
  /* Core functionality methods */

  /*
  This function is called by ASSpreadsheet on a selection change
  Deal with changing the expression in the editor when the selection in the sheet changes
    1) Get the expression at the current location clicked from the evaluation store
    2) Update the state of the evaluation pane, which forces React to rerender (and the editor to rerender)
    3) Treat the special case when the expression is an error/other styles
  */
  _onSelectionChange(rng){
    console.log("Handling selection change: " + JSON.stringify(rng));
    Store.setActiveSelection(rng);
    let cell = Store.getCellAtLoc(rng.col,rng.row),
        {language,expression} = Converter.clientCellGetExpressionObj(cell),
        val = Converter.clientCellGetValueObj(cell);
    // console.log("cell expression: " + expression);
    // here, language is a client/server agnostic object (see Constants.Languages)
    this.setState({ expression: expression, language: Util.getAgnosticLanguageFromServer(language) });
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

  /**************************************************************************************************************************/
  /* The eval pane is the code editor plus the spreadsheet */
  getEditorHeight() { // for future use in resize events
    return Constants.editorHeight + "px";
  },

  getGridHeight() {
    let h = this.props.height - Constants.editorHeight;
    return h + "px";
  },

  render() {
    let {expression, language} = this.state;
    console.log("current expression: " + expression +", language: " + JSON.stringify(language));
    return (
      <div >
        <ASCodeEditor
          ref='editorPane'
          language={language}
          onLanguageChange={this.setLanguage}
          onExpressionChange={this.setExpression}
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
                  message={this.state.toastErrorMessasge}
                  action="Error"
                  onActionTouchTap={this._handleErrorTap} />
      </div>
    );
  }

});
