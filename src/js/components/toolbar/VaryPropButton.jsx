import type {
  NakedRange,
  ASIndex,
  BooleanCellTag,
  ASCell,
  ASCellProp
} from '../../types/Eval';

import React, {PropTypes} from 'react';

import Util from '../../AS/Util';
import API from '../../actions/ASApiActionCreators';

import ToolbarButton from './ToolbarButton.jsx';
import ToolbarController from './ToolbarController.jsx';

/*
This component is for things like the Bold and Italic buttons on the Toolbar
*/

export default React.createClass({

  /* We need the propTag (Bold), and props to pass down to the button control */
  propTypes: {
    propTag: React.PropTypes.string.isRequired,
    tooltip: React.PropTypes.string.isRequired,
    iconName: React.PropTypes.string.isRequired,
  },

  /* When the control updates, toggle the prop in the backend */
  _setBackendCellProp(nextState: boolean, rng: NakedRange) {
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
        let prop = (({ tag: this.props.propTag, contents: []}));
        API.toggleProp(prop,rng);
    }
  },

  /* When the prop updates due to store change, push the button if the prop corresponding to our tag isn't null */
  _setControlStateFromCellProp(prop: ?ASCellProp) {
    console.log("setting state due to store update", prop, prop != null);
    this.refs.button.setPushState(prop != null);
  },

  /* Method that the child control will call after updating its internal state */
  _onClick(nextState: boolean) {
    console.log("in on click in toggle");
    this.refs.controller.onControlStateChange(nextState);
  },

  render() {
    let {iconName, tooltip, buttonControl} = this.props;

    // Define the button control, and note that we always show the tooltip for this component
    let button = 
      <ToolbarButton
        ref="button"
        onClick={this._onClick}  
        iconName={iconName}
        tooltip={tooltip} />;

    return (
      <ToolbarController
        ref="controller"
        propTag={this.props.propTag}
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        setBackendCellProp={this._setBackendCellProp}
        control={button} />
    );
  }

});
