import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

export default {
  // Dropdown was clicked, and it is either visible or not as a result (first argument), and has an ID (second argument)
  click(visible, id) {
    Dispatcher.dispatch({
    	_type: Constants.ActionTypes.DROPDOWN_CLICKED,
     	visible: visible,
     	id: id
    });
  }
};
