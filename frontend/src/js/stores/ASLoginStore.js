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

type LoginState = Immutable.Record$Class;
const LoginRecord = Immutable.Record({
  userId: null,
  loggedIn: false,
  token: null,
  callbacks: [],
  // for callbacks that can only be registered once; they are registered by a provided ID.
  singletonCallbacks: Immutable.Map(),
  isPublicLogin: false
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
        return state.merge({userId, loggedIn: true});
      }

      case 'LOGIN_FAILURE': {
        if (state.loggedIn) {
          window.isLoggedIn = false;
          return state.set('loggedIn', false);
        } else {
          this.__emitChange();
          break;
        }
      }

      case 'LOGIN_CALLBACK_REGISTERED': {
        const {cb, callbackId} = action;
        if (callbackId !== undefined) {
          return state.update('singletonCallbacks', cbs => cbs.set(callbackId, cb));
        } else {
          return state.update('callbacks', cbs => cbs.concat([cb]));
        }
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
    const {callbacks, singletonCallbacks} = this.getState();
    return callbacks.concat(singletonCallbacks.toArray());
  }
}

const devs = ['ritesh@alphasheets.com', 'alex@alphasheets.com', 'anand@alphasheets.com', 'michael@alphasheets.com'];

export default new LoginStore(dispatcher);
