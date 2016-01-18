/* @flow */

import type {
  Callback,
  Dict
} from './Base';

export type ASShortcutTarget =
  'grid' |
  'editor' |
  'evalPane' |
  'toplevel' |
  'evalHeader' |
  'textbox';

export type ASKey = number;

export type ASKeyProperty =
  'shiftKey' |
  'altKey' |
  'ctrlKey' |
  'metaKey';

export type ASKeyModifier = 'Shift' | 'Alt' | 'Ctrl' | 'Meta' | 'Cmd';

export type ASKeyCombination = {
  keyCode: ASKey;
  optionKeys: ?Array<ASKey>;

  shiftKey: boolean;
  ctrlKey: boolean;
  metaKey: boolean;
  altKey: boolean;

  callback: Callback<string>;
};

export type ASShortcut = {
  set: ASShortcutTarget;
  config: Dict<boolean>;
};
