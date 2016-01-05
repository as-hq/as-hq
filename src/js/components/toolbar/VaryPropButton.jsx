/* @flow */

import type {
  ASCellObject,
  NakedRange
} from '../../types/Eval';

import U from '../../AS/Util';
let {
  Conversion: TC
} = U;


import React, {PropTypes} from 'react';
import {Styles} from 'material-ui';

import Util from '../../AS/Util';
import API from '../../actions/ASApiActionCreators';

import ToolbarButton from './ToolbarButton.jsx';
import ToolbarController from './ToolbarController.jsx';

/*
This component is for things like the Bold and Italic buttons on the Toolbar
*/

type VaryPropButtonProps = {
  propTag: string; 
  tooltip: string; 
  iconName: string; 
};

type VaryPropButtonDefaultProps = {};

type VaryPropButtonState = {
  iconColor: string; 
};

export default class VaryPropButton
  extends React.Component<VaryPropButtonDefaultProps, VaryPropButtonProps, VaryPropButtonState>
{
  // NOTE: why money/percent don't work? They have formatType in their cellProps, not tag (like Bold). The filtering in Util.getPropByTag fails.
  /* We need the propTag (Bold), and props to pass down to the button control */
  /* When the control updates, toggle the prop in the backend */
  constructor(props: VaryPropButtonProps) {
    super(props);

    this.state = { 
      iconColor: Styles.Colors.grey500
    };
  }

  _propagateControlStateChange(nextState: VaryPropButtonState, rng: NakedRange) {
    console.log("setting backend");
    // TODO: Not quite the right function to call here
    switch (this.props.propTag) {
      case "Money": 
        API.setFormat("Money", rng);
        break;
      case "Percentage":
        API.setFormat("Percentage", rng);
        break;
      default:
        // $FlowFixMe
        let prop = (({ tag: this.props.propTag, contents: []}));
        API.toggleProp(prop,rng);
    }
  }

  /* When the cell updates due to store change, push the button if the prop corresponding to our tag isn't null */
  _setControlStateFromCell(cell: ?ASCellObject) {
    let prop = (cell != null) ? U.Cell.getPropByTag(this.props.propTag, cell) : null;
    this.refs.button.setPushState(prop != null);
    // We want the bold button's icon to be black upon a bold cell, default color otherwise
    if (this.props.propTag === "Bold"){
      if (prop != null) {
        this.setState({iconColor: Styles.Colors.grey900});
      } else {
        this.setState({iconColor: Styles.Colors.grey500});
      }
    }
  }

  /* Method that the child control will call after updating its internal state */
  _onClick(e: SyntheticMouseEvent, nextState: VaryPropButtonState) {
    console.log("in on click in toggle");
    this.refs.controller.onControlStateChange(nextState);
  }

  render() {
    let {iconName, tooltip} = this.props;

    // Define the button control, and note that we always show the tooltip for this component
    let button = 
      <ToolbarButton
        ref="button"
        onClick={this._onClick.bind(this)}  
        iconName={iconName}
        tooltip={tooltip}
        iconColor={this.state.iconColor} />;

    return (
      <ToolbarController
        ref="controller"
        setControlStateFromCell={this._setControlStateFromCell.bind(this)}
        propagateControlStateChange={this._propagateControlStateChange.bind(this)}
        control={button} />
    );
  }

}

VaryPropButton.propTypes = {
  propTag: React.PropTypes.string.isRequired,
  tooltip: React.PropTypes.string.isRequired,
  iconName: React.PropTypes.string.isRequired,
};
