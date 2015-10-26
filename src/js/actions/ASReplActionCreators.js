import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for the REPL just sends a relevant action to Dispatcher */

export default {

  storeReplExpression(lang,value) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.REPL_LEFT,
      lang: lang,
      value: value
    });
  }


};
