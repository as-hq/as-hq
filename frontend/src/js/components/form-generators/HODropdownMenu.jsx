/* @flow */

import React from 'react';

import ASDropdownMenu from '../basic-controls/ASDropdownMenu.jsx';

type FSelectItem = {
  payload: string;
  text: string;
};

export default function HODropdownMenu(menuItems: Array<FSelectItem>): ReactClass {
  let ret = React.createClass({
    getValueLink(): ReactLink {
      const {valueLink} = this.props;
      const resolvedValue = valueLink.value ? valueLink.value : menuItems[0].payload;

      console.log('RESOLVED VALUE', resolvedValue);

      return ({
        value: resolvedValue,
        requestChange: valueLink.requestChange
      });
    },

    render(): React.Element {
      return (
        <ASDropdownMenu menuItems={menuItems} valueLink={this.getValueLink()} />
      );
    }
  });

  ret.defaultValue = () => menuItems[0].payload;

  return ret;
}
