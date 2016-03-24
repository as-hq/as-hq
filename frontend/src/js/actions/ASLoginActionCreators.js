/* @flow */

import type {Callback} from '../types/Base';

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
    if (API.isTesting) {
      API_test.login();
    } else if (LoginStore.isPublicLogin()) {
      API.loginPublicly();
    } else {
      const token = LoginStore.getToken();
      API.login(token);
    }
  },

  onLoginSuccess(userId: string, sheetId: string) {
    Dispatcher.dispatch({
      _type: 'LOGIN_SUCCESS',
      userId,
      sheetId
    });
  },

  registerCallback(cb: Callback) {
    Dispatcher.dispatch({
      _type: 'LOGIN_CALLBACK_REGISTERED',
      cb
    });
  },

  setPublicLogin() {
    Dispatcher.dispatch({
      _type: 'SET_PUBLIC_LOGIN',
    });
  },
}
