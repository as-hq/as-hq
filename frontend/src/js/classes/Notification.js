/* @flow */

import shortid from 'shortid';
import Immutable from 'immutable';

import type {
  Callback
} from '../types/Base';

type NotificationLevel = 'success' | 'error' | 'warning' | 'info';

type NotificationAction = {
  label: string;
  callback: Callback;
};

type NotificationPosition = 'tr' | 'tl' | 'tc' | 'br' | 'bl' | 'bc';

export type NotificationSpec = {
  uid: ?string;
  title: string;
  autoDismiss: ?number;
  message: ?string;
  level: NotificationLevel;
  action: ?NotificationAction;
  position: ?NotificationPosition;
  dismissible: ?boolean;
};

export type Notification = Immutable.Record$Class;

export default Immutable.Record({
  uid: shortid.generate(),
  title: 'DefaultTitle',
  autoDismiss: 0,
  message: null,
  level: 'success',
  action: null,
  position: 'br',
  dismissible: true
});
