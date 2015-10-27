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
  }

};
