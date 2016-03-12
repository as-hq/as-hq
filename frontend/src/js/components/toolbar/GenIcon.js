// @flow

import React from 'react';
import {Styles, FontIcon} from 'material-ui';

export default function GenIcon({name}: { name: string; }): React.Element {
  return (
    <FontIcon
      style={{
        backgroundColor: Styles.Colors.grey400,
        color: Styles.Colors.grey800
      }}
      className="material-icons"
    >
      {name}
    </FontIcon>
  );
}
