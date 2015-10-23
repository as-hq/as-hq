import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for pressing enter or up/down in  find bar, need to re-update the position ("1 of 4") of the find bar*/

export default {

  incrementSelection() {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.FIND_INCREMENTED,
    });
  },
  decrementSelection(locs) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.FIND_DECREMENTED,
    });
  }
};
