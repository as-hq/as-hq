/* @flow */

import type {
  NakedRange,
  ASIndex,
  BooleanCellTag,
  ASCell,
  ASCellProp
} from '../../types/Eval';

import React, {PropTypes} from 'react';
import ASButton from './ASButton.jsx';

import Util from '../../AS/Util';
import TC from '../../AS/TypeConversions';
import API from '../../actions/ASApiActionCreators';
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

export class ASCellPropsToggleButton
  extends React.Component<{}, ToggleButtonProps, ToggleButtonState>
{
  getInitialState(): ToggleButtonState {
    return ({
      active: false
    });
  }

  _setBackendCellProp(rng: NakedRange) {
    let prop = (({ tag: this.props.propTag }): any);
    API.toggleProp(prop, rng);
  }

  _setControlStateFromCellProp(prop: ?ASCellProp) {
    this.setState({active: !!prop});
  }

  componentDidUpdate(prevProps: ToggleButtonProps, prevState: ToggleButtonState) {
    this.refs.controller.onControlStateChange();
  }

  render(): React.Element {
    let {iconClassName, propTag, ...etc} = this.props;
    let {active} = this.state;

    return (
      <ASCellPropControl
        ref="controller"
        control={<ASButton
          height="24px"
          primary={active}
          iconClassName={iconClassName}
          {...etc}
        />}
        setBackendCellProp={this._setBackendCellProp}
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        propTag={this.props.propTag}/>
    );
  }
}

ASCellPropsToggleButton.propTypes = {
  iconClassName: React.PropTypes.string.isRequired,
  propTag: React.PropTypes.string.isRequired
};
