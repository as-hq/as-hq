/* @flow */

import type {
  ASOverlaySpec
} from '../types/Hypergrid';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for expression changes */

export default {

  add(ovl: ASOverlaySpec) {
    Dispatcher.dispatch({
     _type: 'ADD_OVERLAY',
      overlay: ovl
    });
  }

}
