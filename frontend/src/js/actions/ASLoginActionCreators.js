/* @flow */

import API from './ASApiActionCreators';
import { API_test } from './ASApiActionCreators';
import Dispatcher from '../Dispatcher';
import LoginStore from '../stores/ASLoginStore';

export default {
  login(token: string) {
    API.login(token);
    Dispatcher.dispatch({
      _type: 'LOGIN_ATTEMPT',
      token
    });
  },

  relogin() {
    if (! API.isTesting) {
      const token = LoginStore.getToken();
      API.login(token);
    } else {
      API_test.login();
    }
  },

  onLoginSuccess(userId: string, sheetId: string) {
    Dispatcher.dispatch({
      _type: 'LOGIN_SUCCESS',
      userId,
      sheetId
    });
  }
}
