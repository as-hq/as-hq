/* @flow */

import type {
  BooleanCellTag,
  ASCellProp
} from '../../types/Eval';

import React, {PropTypes} from 'react';
import ASButton from './ASButton.jsx';
import ASCheckedButton from './ASCheckedButton.jsx';

import Util from '../../AS/Util';
import API from '../../actions/ASApiActionCreators';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import SelectionStore from '../../stores/ASSelectionStore';

import ASCellPropControl from './ASCellPropControl.jsx';

type ToggleButtonProps = {
  iconClassName: string;
  propTag: BooleanCellTag;
};

type ToggleButtonState = {
  active: boolean;
};

export default React.createClass({
  propTypes: {
    iconClassName: React.PropTypes.string.isRequired,
    propTag: React.PropTypes.string.isRequired
  },

  getInitialState(): ToggleButtonState {
    return ({
      active: false
    });
  },

  _setBackendCellProp(nextState: boolean, rng: ASRange) {
    let prop = (({ tag: this.props.propTag, contents: []}): any);
    API.toggleProp(prop, rng);
    // TODO: make this reflect current state, rather than assuming that
    // state changes <--> prop changes. (Even though that should always
    // be true in principle, it would be more stable if we didn't have
    // to assume this.)
  },

  _setControlStateFromCellProp(prop: ?ASCellProp) {
    this.setState({active: prop !== null});
  },

  _onTouchTap() {
    this.refs.controller.onControlStateChange(!this.state.active);
  },

  render(): React.Element {
    let {iconClassName, propTag, ...etc} = this.props;
    let {active} = this.state;

    return (
      <ASCellPropControl
        ref="controller"
        control={<ASCheckedButton
          checked={active}
          iconClassName={iconClassName}
          onTouchTap={this._onTouchTap}
          {...etc}
        />}
        setBackendCellProp={this._setBackendCellProp}
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        propTag={this.props.propTag}/>
    );
  }
});
