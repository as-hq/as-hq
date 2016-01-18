import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for expression changes */

export default {

  add(ovl) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.ADD_OVERLAY,
      overlay: ovl
    });
  }

}
