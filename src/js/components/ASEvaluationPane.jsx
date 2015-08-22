import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import ASEvaluationStore from '../stores/ASEvaluationStore';
import API from '../actions/ASApiActionCreators';
import Shortcuts from '../AS/Shortcuts';
import Util from '../AS/Util';
import Constants from '../Constants';
import Converter from '../AS/Converter'
import KeyUtils from '../AS/KeyUtils';

var NotificationSystem = require('react-notification-system');


export default React.createClass({

  /* Used to create error messages */
  _notificationSystem: null,
  _numNotifications: 0,
  maxNotifs: 3,


  /***************************************************************************************************************************/
  /* State methods */

  /* React method for getting the initial state */
  getInitialState() {
    return {
      expression: '',
      language: Constants.Languages.Python,
      focus: 'grid'
    };
  },
  setLanguage(lang) {
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
  /* Getter methods for "children" components of the eval pane */

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
    ASEvaluationStore.addChangeListener(this._onChange);
    this._notificationSystem = this.refs.notificationSystem;
    this.addShortcuts();
  },
  componentWillUnmount() {
    ASEvaluationStore.removeChangeListener(this._onChange);
  },
  addError(cv){
    if (cv.tag === "ValueError"){
      if (this._numNotifications < this.maxNotifs){
        this._notificationSystem.addNotification({
              message: cv.error,
              level: 'error',
              autoDismiss: 10,
              onRemove: function(){this._numNotifications -= 1;}

            });
        this._numNotifications += 1;
      }
    }
  },
  /*
  Upon a change event from the eval store (for example, eval has already happened)
    1) Get the last updated cells
    2) Call a ASSpreadsheet component method that forces an update of values
    3) Treat the special case of errors/other styles
  */
  _onChange() {
    console.log("Eval pane detected event change from store");
    let updatedCells = ASEvaluationStore.getLastUpdatedCells();
    console.log("Updated cells: " + JSON.stringify(updatedCells));
    this.refs.spreadsheet.updateCellValues(updatedCells);
    for (var key in updatedCells){
      let cv = Converter.clientCellGetValueObj(updatedCells[key]);
      this.addError(cv);
    }
  },

  /**************************************************************************************************************************/
  /* Keyboard shortcuts */

  addShortcuts() {
    // TODO
    Shortcuts.addShortcut("common", "toggle_focus", "F2", this.toggleFocus());
    Shortcuts.addShortcut("common", "cell_eval", ["Ctrl+Enter", "Command+Enter"], (wildcard) => {
      let editorState = {
        exp: this._getRawEditor().getValue(),
        lang: this.state.language
      };
      this.handleEvalRequest(editorState);
    });
    Shortcuts.addShortcut("common", "set_language", ["Ctrl+1/2/3/4/5/6/7/8/9", "Command+1/2/3/4/5/6/7/8/9"], (wildcard) => {
      switch(wildcard) {
          case "1":
            this.setLanguage(Constants.Languages.Excel);
            break;
          case "2":
            this.setLanguage(Constants.Languages.Python);
            break;
          case "3":
            this.setLanguage(Constants.Languages.R);
            break;
          case "4":
            this.setLanguage(Constants.Languages.OCaml);
            break;
          case "5":
            this.setLanguage(Constants.Languages.SQL);
            break;
          case "6":
            this.setLanguage(Constants.Languages.Java);
            break;
          case "7":
            this.setLanguage(Constants.Languages.CPP);
            break;
        }
    })
  },

  _onEditorDeferredKey(e) {
    console.log('trying common shortcut');
    console.log(e);
    Shortcuts.tryCommonShortcut(e);
    // TODO bubble up editor event
  },

  _onGridDeferredKey(e) {
    if (KeyUtils.producesVisibleChar(e)) {
      console.log("key deferred by grid to editor");
      console.log(e);
      let editor = this._getRawEditor(),
          str = Shortcuts.modifyStringForKey(editor.getValue(), e);
      console.log(str);
      if (str || str === "")
        editor.setValue(str);
    }
    else {
      console.log('trying common shortcut');
      console.log(e);
      Shortcuts.tryCommonShortcut(e);
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
    let cell = ASEvaluationStore.getCellAtLoc(rng.col,rng.row);
    let {language,expression} = Converter.clientCellGetExpressionObj(cell);
    let val = Converter.clientCellGetValueObj(cell);
    this.setState({ expression: expression, language: language });
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
          onToggle={this.toggleFocus}
          width="100%"
          height="100%"  />
        <NotificationSystem ref="notificationSystem" />
      </div>
    );
  },

});
