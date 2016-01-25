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
  title: string;
  message: string;
  level: NotificationLevel;
  action: NotificationAction;
}
