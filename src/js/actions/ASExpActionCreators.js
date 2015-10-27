import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for expression changes */

export default {

  handleEditorChange(xpStr) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.EDITOR_CHANGED,
      xpStr: xpStr
    });
  },

  handleTextBoxChange(xpStr){
    Dispatcher.dispatch({
      type: Constants.ActionTypes.TEXTBOX_CHANGED,
      xpStr: xpStr
    });
  }


};
