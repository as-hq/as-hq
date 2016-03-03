/* @flow */

import type {
  ASOverlaySpec
} from '../types/Overlay';

import Dispatcher from '../Dispatcher';

export default {

  add(overlay: ASOverlaySpec) {
    Dispatcher.dispatch({
     _type: 'ADD_OVERLAY_WITHOUT_LOC',
      overlay
    });
  },

  resize(overlay: ASOverlaySpec, width: number, height: number) {
  	Dispatcher.dispatch({
  	 _type: 'OVERLAY_RESIZED',
  	  overlay,
  	  width,
  	  height
  	});
  },

  delete(overlay: ASOverlaySpec) {
  	Dispatcher.dispatch({
  	 _type: 'OVERLAY_DELETED',
  	  overlay
  	});
  }

}
