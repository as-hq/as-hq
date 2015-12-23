import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for pressing enter or up/down in  find bar, need to re-update the position ("1 of 4") of the find bar*/

export default {

  // Dropdown was clicked, and it is either visible or not as a result (first argument), and has an ID (second argument)
  click(visible, id) {
    Dispatcher.dispatch({
    	_type: Constants.ActionTypes.DROPDOWN_CLICKED,
     	visible: visible,
     	id: id
    });
  },

  toggleLanguage(lang){
  	Dispatcher.dispatch({
  		_type: Constants.ActionTypes.LANGUAGE_TOGGLED,
  		lang: lang
  	});
  }
  
};
