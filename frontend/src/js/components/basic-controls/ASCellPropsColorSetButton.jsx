/* @flow */

import type {
  ASCellProp
} from '../../types/Eval';

import React, {PropTypes} from 'react';

import Constants from '../../Constants';
import Util from '../../AS/Util';
import API from '../../actions/ASApiActionCreators';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import SelectionStore from '../../stores/ASSelectionStore';

import ASColorPicker from './ASColorPicker.jsx';
import ASCellPropControl from './ASCellPropControl.jsx';

type ColorSetButtonProps = {
  propTag: string;
};

type ColorSetButtonState = {
  color: string;
};

export default React.createClass({
  propTypes: {
    propTag: React.PropTypes.string.isRequired
  },

  getInitialState(): ColorSetButtonState {
    return ({
      color: '#000000'
    });
  },

  _setBackendCellProp(nextState: string, rng: ASRange) {
    API.setProp({
      // $FlowFixMe: this one's similar to the boolean one
      tag: this.props.propTag,
      contents: nextState
    }, rng);
  },

  _setControlStateFromCellProp(prop: ?ASCellProp) {
    let newColor = Constants.DefaultColors[this.props.propTag];
    if (prop !== null && prop !== undefined) {
      if (prop.tag !== 'FillColor' && prop.tag !== 'TextColor') {
        throw new Error('Color set button received non-color prop');
      }
      newColor = prop.contents;
    }

    this.setState({ color: newColor });
  },

  _onValueChange(newValue: string) {
    this.refs.controller.onControlStateChange(newValue);
  },

  render(): React.Element {
    let {propTag, ...etc} = this.props;
    let {color} = this.state;

    return (
      <ASCellPropControl
        ref="controller"
        control={
          <ASColorPicker
            ref="colorPicker"
            value={color}
            onValueChange={this._onValueChange}
            {...etc} />
        }
        setBackendCellProp={this._setBackendCellProp}
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        propTag={this.props.propTag}/>
    );
  }
});
