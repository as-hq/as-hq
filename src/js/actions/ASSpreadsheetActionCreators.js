import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for the spreadsheet just sends a relevant action to Dispatcher */

export default {

  selectCell(loc) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.CELL_CHANGED,
      loc: loc
    });
  },
  selectRange(locs) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.RANGE_CHANGED,
      locs: locs
    });
  },
  /* Dispatches a scroll event. Called from fin-hypergrid event listener in ASSpreadsheet */
  scroll(vWindow) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.SCROLLED,
      vWindow: vWindow
    });
  }
};
