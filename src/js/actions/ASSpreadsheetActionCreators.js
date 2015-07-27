import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

export default {
  selectCell(loc) {
    Dispatcher.handleViewAction({
      type: Constants.ActionTypes.CELL_CHANGED,
      loc: loc
    });
  },

  selectRange(locs) {
    Dispatcher.handleViewAction({
      type: Constants.ActionTypes.RANGE_CHANGED,
      locs: locs
    });
  },

  scroll(viewingWindow) {
    Dispatcher.handleViewAction({
      type: Constants.ActionTypes.SCROLLED,
      viewingWindow: viewingWindow
    });
  }
};
