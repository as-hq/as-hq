/* @flow */

import type {
  NestedMenuSpec,
  SimpleItemSpec,
  MenuItemSpec
} from './menu-bar/types';

import React from 'react';

import ASMenuBar from './menu-bar/ASMenuBar.jsx';

function nested(etc): NestedMenuSpec {
  return ({
    tag: 'NestedMenuSpec',
    ...etc
  });
}

function simple({callback, title}): SimpleItemSpec {
  return ({
    tag: 'SimpleItemSpec',
    title: title,
    callback: callback
  });
}

export default function ASTopBar(props: {}): React.Element {
  return (
    <ASMenuBar menus={[
      {title: 'File', menuItems: [
        simple({
          title: 'test',
          callback() {
            alert('test');
          }
        })
      ]},

      {title: 'Edit', menuItems: [
        simple({
          title: 'test',
          callback() {
            alert('test');
          }
        })
      ]}
    ]} />
  );
}
