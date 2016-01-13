/* @flow */

import type {
  Callback
} from './Base';

import BaseStore from '../stores/BaseStore';

export type StoreLink = {
  store: typeof BaseStore;
  listener: Callback;
};
