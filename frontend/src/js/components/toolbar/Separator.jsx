/* @flow */

import React from 'react';

export default class Separator extends React.Component<{}, {}, {}> {

  shouldComponentUpdate(nextProps: {}, nextState: {}): boolean { 
    return false;
  }

  render(): React.Element {
    return <div style={style} />;
  }
}

// Used to create a separating element between parts of toolbar
// There is a ToolbarSeparator in material-ui but it didn't quite fit the bill; a simple div we control is better
const style = {
  display: 'inline-block',
  height: 50, // TODO(joel) this.props.toolbarHeight,
  marginLeft: 10, // equal separation distance on both sides
  marginRight: 10,
  backgroundColor: '#202020',
  boxShadow: '0px 0px 1px 0px rgba(255, 255, 255, 0.35)',
  verticalAlign: 'top', // we want the separator to span the height of the whole pane
  width: 2
};