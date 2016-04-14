/* @flow */

// Tracks the login state of the user.

import type {
  ASAction
} from '../types/Actions';

import type { Callback } from '../types/Base';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import API from '../actions/ASApiActionCreators';

import invariant from 'invariant';
import dispatcher from '../Dispatcher';

import shortid from 'shortid';
import _ from 'lodash';

type LoginState = any;
const LoginRecord = Immutable.Record({
  userId: null,
  loggedIn: false,
  token: null,
  callbacks: [],
  isPublicLogin: false,
  reloginAttempts: 0
});

class LoginStore extends ReduceStore<LoginState> {

  getInitialState(): LoginState {
    return new LoginRecord();
  }

  reduce(state: LoginState, action: ASAction): LoginState {
    switch (action._type) {
      case 'LOGIN_ATTEMPT': {
        const {token} = action;
        return state.set('token', token);
      }

      case 'LOGIN_SUCCESS': {
        const {userId} = action;
        console.warn('Login success, got userId: ', userId);
        window.isLoggedIn = true;
        return state.merge({userId, loggedIn: true, reloginAttempts: 0});
      }

      case 'LOGIN_FAILURE': {
        window.isLoggedIn = false;
        return state.set('loggedIn', false)
                    .update('reloginAttempts', n => n + 1);
      }

      case 'LOGIN_CALLBACK_REGISTERED': {
        const {cb} = action;
        return state.update('callbacks', cbs => cbs.concat([cb]));
      }

      case 'SET_PUBLIC_LOGIN': {
        return state.set('isPublicLogin', true);
      }

      default: {
        return state;
      }
    }
  }

  isLoggedIn(): boolean {
    return this.getState().loggedIn;
  }

  isPublicLogin(): boolean {
    return this.getState().isPublicLogin;
  }

  getUserId(): string {
    const uid = this.getState().userId;
    invariant(uid, 'Authenticated user does not have a user ID!');
    return uid;
  }

  getToken(): string {
    const token = this.getState().token;
    invariant(token, 'Token not found for authenticated user');
    return token;
  }

  userIsDev(): boolean {
    const userId = this.getUserId();
    return _.includes(devs, userId);
  }

  getCallbacks(): Array<Callback> {
    return this.getState().callbacks;
  }

  reloginAttemptsExceeded(): boolean {
    return this.getState().reloginAttempts > maxReloginAttempts;
  }
}

const maxReloginAttempts = 5;
const devs = ['ritesh@alphasheets.com', 'alex@alphasheets.com', 'anand@alphasheets.com', 'michael@alphasheets.com'];

export default new LoginStore(dispatcher);
