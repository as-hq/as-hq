import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for the spreadsheet just sends a relevant action to Dispatcher */

export default {

  /* Dispatches a scroll event. Called from fin-hypergrid event listener in ASSpreadsheet */
  scroll(vWindow) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.SCROLLED,
      vWindow: vWindow
    });
  }
};
