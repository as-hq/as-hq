/* @flow */

import type {
  Callback
} from './Base';

export type NotificationLevel = 'success' | 'error' | 'warning';

export type NotificationAction = {
  label: string;
  callback: Callback;
}

export type NotificationSpec = {
  uid: string;
  title: string;
  message: string;
  level: NotificationLevel;
  action: NotificationAction;
}
