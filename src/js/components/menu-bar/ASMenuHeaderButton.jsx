/* @flow */

import type {
  ASMenuHeaderButtonProps
} from './types';

import React from 'react';

import _Styles from '../../styles/menu-bar/ASMenuBar';

import ASButton from '../basic-controls/ASButton.jsx';

export default class ASMenuHeaderButton extends React.Component<{}, ASMenuHeaderButtonProps, {}> {
  render(): React.Element {
    let {title} = this.props;

    return (
      <ASButton
        label={title}
        style={_Styles.menu} />
    );
  }
}
