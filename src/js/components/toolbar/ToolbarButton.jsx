import React from 'react';

import {Styles, FontIcon} from 'material-ui';
const FlatButton = require('material-ui/lib/flat-button');
import DropDownArrow from 'material-ui/lib/svg-icons/navigation/arrow-drop-down';

/*
This component displays an icon in a button
It also has the ability to display a tooltip (blurb of what the button does upon hovering)
*/

export default React.createClass({

  /*************************************************************************************************************************/
  // React methods

  /*
    We keep the following props:
      1) size of the square button
      2) spacing (marginLeft)
      3) onClick callback
      4) iconName: name of the material-ui icon used for the FontIcon (print, home, etc)
      5) iconElement: HTML Element of the icon itself can be passed; overrides iconName if not null
      6) tooltipId: ID of the tooltip element in Toolbar
      7) tooltip: actual message underneath button (ex. Print (Ctrl+P))
      8) usePushState: should the button "stay pushed" when pushed; should its color and state change?
         For buttons like undo and redo, this is false (no pushed state needed), for bold, this is needed, since
         the button should be pushed in if the active cell/selection is bold
      9) Should we show the tooltip
  */
  propTypes: {
    size: React.PropTypes.number, 
    spacing: React.PropTypes.number, 
    onClick: React.PropTypes.func, 
    iconName: React.PropTypes.string, 
    iconElement: React.PropTypes.object, 
    tooltipId: React.PropTypes.string, 
    tooltip: React.PropTypes.string, 
    usePushState: React.PropTypes.boolean,
    showTooltip: React.PropTypes.boolean
  },

  getDefaultProps() {
    return {
      size: 36,
      spacing: 0,
      onClick: (state) => {},
      iconName: 'home',
      iconElement: null,
      usePushState: true,
      showTooltip: true
    };
  },

  /* 
    We keep the following state:
      1) pushed; is the button pushed?
  */
  getInitialState() {
    return {
      pushed: false
    }
  },

  /*************************************************************************************************************************/
  // Respond to events

  // Update internal state and tell parent about it
  _onClick(e) {
    if (this.props.usePushState) {
      console.log("\n\npushing");
      let nextState = !this.state.pushed;
      this.setState({pushed: nextState});
      this.props.onClick(nextState);
    } else {
      this.props.onClick(this.state.pushed);
    }
  },

  // Push the button (upon parent's request) 
  setPushState(shouldBePushed) {
    if (this.props.usePushState) {
      this.setState({pushed: shouldBePushed});
    }
  },

  /*************************************************************************************************************************/
  // Render

  render() {

    // Background color of each button upon hovering over it
    let hoverColor = Styles.Colors.pink300;
    // Color when pushed
    let pushedColor = Styles.Colors.green300;

    let buttonStyle = {
      display: 'inline-block', // buttons should stack horizontally
      position: 'relative', 
      top: '50%', 
      transform: 'translateY(-50%)', 
      width: this.props.size, // square buttons that have spacing on top and bottom
      minWidth: this.props.size, // needed to make override current minWidth, which prevents width from having an effect if too small
      height: this.props.size,
      marginLeft: this.props.spacing, // spacing between buttons
      // background color gets in the way of hoverColor and the default is already set by toolbarStyle
    };

    if (this.state.pushed) {
      console.log("setting background color");
      buttonStyle.backgroundColor = pushedColor;
    }
 
    // Style that centers an absolute icon horizontally and vertically within a button
    // I can't figure out how to style their sizes (Ritesh 12/16)
    let iconStyle = { 
      position: 'absolute',
      top: '50%',
      left: '50%',
      transform: 'translate(-50%, -50%)'
    };

    // If the user passes in their own icon element, use that, or use their material-ui icon, or default to print-icon
    let iconElement = (this.props.iconElement == null) ? 
      <FontIcon style={iconStyle} className="material-icons"> {this.props.iconName} </FontIcon>
      : this.props.iconElement;

    // Display the tooltip only if the user says so, and if the control isn't visible
    let buttonElement = (this.props.showTooltip) ? 
      <FlatButton 
        data-for={this.props.tooltipId}
        data-tip={this.props.tooltip}
        hoverColor={hoverColor}
        style={buttonStyle}
        onClick={this._onClick}>
        {iconElement}
      </FlatButton>
    : <FlatButton 
        hoverColor={hoverColor}
        style={buttonStyle}
        onClick={this._onClick}>
        {iconElement}
      </FlatButton>;

    return buttonElement;
  }

  
});
