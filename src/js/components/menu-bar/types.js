/* @flow */

import type {
  Callback
} from '../../types/Base';

export type NestedMenuSpec = {
  tag: 'NestedMenuSpec';
  title: string;
  menuItems: Array<MenuItemSpec>;
};

export type SimpleItemSpec = {
  tag: 'SimpleItemSpec';
  title: string;
  callback: Callback;
};

export type MenuItemSpec = NestedMenuSpec | SimpleItemSpec;

export type MenuSpec = {
  title: string;
  menuItems: Array<MenuItemSpec>;
};

export type ASMenuBarProps = {
  menus: Array<MenuSpec>;
  style: {[key: string]: any};
};

export type ASMenuBarState = {
  currentMenuIdx: number;
};

export type ASMenuProps = {
  open: boolean;
  title: string;
  menuItems: Array<MenuItemSpec>;
  onClick: Callback;
  onHover: Callback;
  onRequestClose: Callback;
};

export type ASMenuState = {
  anchor: ?HTMLElement;
};

export type ASMenuHeaderButtonProps = {
  title: string;
};
