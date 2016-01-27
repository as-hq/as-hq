import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

import API from '../actions/ASApiActionCreators';

import ExpStore from '../stores/ASExpStore';
import SelectionStore from '../stores/ASSelectionStore';

/* The action creator for expression changes */

export default {

  // Handle a change originating from the ace editor
  // xpStr is the new string
  handleEditorChange(xpStr) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.EDITOR_CHANGED,
      xpStr: xpStr
    });
  },

  // Textbox onkeyup fired, xpStr is the new string
  handleTextBoxChange(xpStr) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.TEXTBOX_CHANGED,
      xpStr: xpStr
    });
  },

  // Grid key pressed; xpStr is the updated str
  // namely, the old string with a new character at the end
  handleGridChange(xpStr, cursorPos) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.GRID_KEY_PRESSED,
      xpStr: xpStr,
      cursorPos: cursorPos
    });
  },

  handleSelChange(xpStr) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.NORMAL_SEL_CHANGED,
      xpStr: xpStr
    });
  },

  handlePartialRefEditor(xpStr,excelStr) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_EDITOR,
      xpStr: xpStr,
      excelStr: excelStr
    });
  },

  handlePartialRefTextBox(xpStr,excelStr) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_TEXTBOX,
      xpStr: xpStr,
      excelStr: excelStr
    });
  },

  handlePartialRefGrid(excelStr) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID,
      excelStr: excelStr
    });
  },

  handleEscape() {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.ESC_PRESSED
    });
  },

  handleToggleLanguage(lang) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.LANGUAGE_TOGGLED,
     lang: lang
    });

    if (!ExpStore.getUserIsTyping() && ExpStore.getExpression() !== '') {
      SelectionStore.withActiveSelection(({range}) => API.setLanguagesInRange(lang, range)); 
    }
  }
};
