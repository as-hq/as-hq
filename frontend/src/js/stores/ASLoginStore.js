/* @flow */

// Tracks the login state of the user.

import type {
  ASAction
} from '../types/Actions';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';

// $FlowFixMe
import invariant from 'invariant';
import dispatcher from '../Dispatcher';

import shortid from 'shortid';
import _ from 'lodash';

type LoginState = Immutable.Record$Class;
const LoginRecord = Immutable.Record({userId: null, loggedIn: false, token: null});

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
        return state.merge({userId, loggedIn: true});
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
    const devs = ['ritesh@alphasheets.com', 'alex@alphasheets.com', 'anand@alphasheets.com', 'michael@alphasheets.com'];
    return _.contains(devs, userId);
  }
}

export default new LoginStore(dispatcher);
