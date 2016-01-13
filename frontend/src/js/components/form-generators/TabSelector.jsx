/* @flow */

// menuItems and valueLink

import React from 'react';

// $FlowFixMe
import {Tabs, Tab} from 'material-ui';

type MenuItem = {
  payload: string;
  text: string;
};

type TabSelectorProps = {
  menuItems: Array<MenuItem>;
  valueLink: ReactLink;
};

export default function TabSelector(props: TabSelectorProps): React.Element {
  const {
    valueLink: {value, requestChange},
    menuItems
  } = props;

  return (
    <Tabs value={value} onChange={(val) => requestChange(val)}>
      {menuItems.map(({text, payload}) =>
        <Tab label={text} value={payload} />
      )}
    </Tabs>
  );
}
