// @flow
import React, { Component } from 'react';
import { findDOMNode } from 'react-dom';

import type { ReactElement } from 'react';

function isDescendant(parent: Node, child: Node): boolean {
  let node: Node = child.parentNode;

  while (node != null) {
    if (node === parent) {
      return true;
    }

    node = node.parentNode;
  }

  return false;
}

type ClickAwayProps = {
  children: ReactElement;
  componentClickAway: () => void;
};

// Higher-order (non-mixin) replacement for react-clickaway.
//
// Fires when you click away from the child component.
//
// Example:
//
// <ClickAway componentClickAway={handleClickAway}>
//   <Dialog />
// </ClickAway>
export default class ClickAway extends Component<{}, ClickAwayProps, {}> {
  componentDidMount() {
    document.addEventListener('onclick', this._checkClickAway);
  }

  componentWillUnmount() {
    document.removeEventListener('onclick', this._checkClickAway);
  }

  _checkClickAway(event) {
    const el = findDOMNode(this);
    const { target } = event;

    if (target !== el
        && !isDescendant(el, target)
        && document.documentElement.contains(target)
       ) {
      this.props.componentClickAway();
    }
  }

  render() {
    const { children, componentClickAway, ...props } = this.props;
    const Child = React.Children.only(this.props.children);

    return <Child {...props} />;
  }
}
