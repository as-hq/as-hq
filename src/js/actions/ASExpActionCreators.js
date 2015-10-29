import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for expression changes */

export default {

  // Handle a change originating from the ace editor
  // xpStr is the new string
  handleEditorChange(xpStr) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.EDITOR_CHANGED,
      xpStr: xpStr
    });
  },

  // Textbox onkeyup fired, xpStr is the new string
  handleTextBoxChange(xpStr){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.TEXTBOX_CHANGED,
      xpStr: xpStr
    });
  },

  // Grid key pressed; xpStr is the updated str
  // namely, the old string with a new character at the end
  handleGridChange(xpStr){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.GRID_KEY_PRESSED,
      xpStr: xpStr
    });
  }, 

  handleSelChange(xpStr){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.NORMAL_SEL_CHANGED,
      xpStr: xpStr
    });
  },

  handlePartialRefEditor(xpStr,excelStr){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_EDITOR,
      xpStr: xpStr,
      excelStr:excelStr
    });
  },

  handlePartialRefTextBox(xpStr,excelStr){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_TEXTBOX,
      xpStr: xpStr,
      excelStr:excelStr
    });
  },

  handlePartialRefGrid(excelStr){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID,
      excelStr:excelStr
    });
  },

  handleEscape(){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.ESC_PRESSED,
    });
  }

};
