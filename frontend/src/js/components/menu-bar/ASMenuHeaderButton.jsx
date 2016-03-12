/* @flow */

import type {
  ASMenuHeaderButtonProps
} from './types';

import React from 'react';

import {FlatButton} from 'material-ui';

import _Styles from '../../styles/menu-bar/ASMenuBar';

// import ASButton from '../basic-controls/ASButton.jsx';

export default class ASMenuHeaderButton extends React.Component {
  static defaultProps = {}; 
  props: ASMenuHeaderButtonProps;
  state: {};

  render(): React.Element {
    let {title, ...etc} = this.props;

    return (
      <FlatButton
        label={title}
        style={_Styles.menu}
        {...etc} />
    );
  }
}
