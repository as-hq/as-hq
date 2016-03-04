/* @flow */

// Tracks the login state of the user.

import type {
  ASAction
} from '../types/Actions';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import API from '../actions/ASApiActionCreators';

// $FlowFixMe
import invariant from 'invariant';
import dispatcher from '../Dispatcher';

import shortid from 'shortid';
import _ from 'lodash';

type LoginState = Immutable.Record$Class;
const LoginRecord = Immutable.Record({
  userId: null,
  loggedIn: false,
  token: null
});

class LoginStore extends ReduceStore<LoginState> {

  getInitialState(): LoginState {
    return new LoginRecord();
  }

  reduce(state: LoginState, action: ASAction): LoginState {
    switch (action._type) {
      case 'LOGIN_ATTEMPT': {
        const {token} = action;
        return new LoginRecord({token});
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

      default: {
        return state;
      }
    }
  }

  isLoggedIn(): boolean {
    return this.getState().loggedIn;
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
    return _.contains(devs, userId);
  }
}

const devs = ['ritesh@alphasheets.com', 'alex@alphasheets.com', 'anand@alphasheets.com', 'michael@alphasheets.com'];

export default new LoginStore(dispatcher);
