import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

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

  scroll(x, y, vWindow) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.SCROLLED,
      vWindow: vWindow,
      xscroll: x,
      yscroll: y
    });
  }
};
