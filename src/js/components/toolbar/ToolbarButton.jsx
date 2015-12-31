/* @flow */

import type {
  ASCell,
  ASRange
} from '../../types/Eval';

import React from 'react';

import {Styles, FontIcon} from 'material-ui';
// $FlowFixMe
const FlatButton = require('material-ui/lib/flat-button');
// $FlowFixMe
import DropDownArrow from 'material-ui/lib/svg-icons/navigation/arrow-drop-down';
// $FlowFixMe
let Tooltip = require("react-tooltip");



/*
This component displays an icon in a button
It also has the ability to display a tooltip (blurb of what the button does upon hovering)
*/

type ToolbarButtonDefaultProps = {
  width: number;
  height: number;
  spacing: number; 
  onClick: (e: SyntheticMouseEvent, state: any) => void; 
  iconName: string; 
  iconElement: ?React.Element;
  tooltip: string; 
  usePushState: boolean;
  showTooltip: boolean;
  includeDropdownArrow: boolean;
  arrowSize: number;
  iconColor: string;
};

type ToolbarButtonProps = {
  width: number;
  height: number;
  spacing: number; 
  onClick: (e: SyntheticMouseEvent, state: any) => void; // #needsrefactor only any because type of second arg depends on a prop...
  iconName: string; 
  iconElement: ?React.Element;
  tooltip: string; 
  usePushState: boolean;
  showTooltip: boolean;
  includeDropdownArrow: boolean;
  arrowSize: number;
  iconColor: string;
};

type ToolbarButtonState = {
  pushed: boolean; 
  hovered: boolean; 
};

export default class ToolbarButton
  extends React.Component<ToolbarButtonDefaultProps, ToolbarButtonProps, ToolbarButtonState>
{

  /*************************************************************************************************************************/
  // React methods

  /*
    We keep the following props:
      1/2) size of the button
      3) spacing (marginLeft)
      4) onClick callback function (e, nextState) => {}
      5) iconName: name of the material-ui icon used for the FontIcon (print, home, etc)
      6) iconElement: HTML Element of the icon itself can be passed; overrides iconName if not null
      7) tooltip: actual message underneath button (ex. Print (Ctrl+P))
      8) usePushState: should the button "stay pushed" when pushed; should its color and state change?
         For buttons like undo and redo, this is false (no pushed state needed), for bold, this is needed, since
         the button should be pushed in if the active cell/selection is bold
      9) Should we show the tooltip
      10) Should we include a dropdown arrow inside the button? This wouldn't do anything by itself if clicked.
      11) Size of that arrow
      12) Color of the icon
  */

  /* 
    We keep the following state:
      1) pushed; is the button pushed?
  */

  constructor(props: ToolbarButtonProps) {
    super(props);

    this.state = { 
      pushed: false, 
      hovered: false
    }
  }

  /*************************************************************************************************************************/
  // Respond to events

  // Update internal state and tell parent about it. Separator shouldn't be visible when pushed
  _onClick(e: SyntheticMouseEvent) {
    console.log("Toolbar button onclick");
    if (this.props.usePushState) {
      let nextState = !this.state.pushed;
      this.setState({pushed: nextState});
      this.props.onClick(e, nextState);
    } else {
      this.props.onClick(e, this.state.pushed);
    }
  }

  // Push the button (upon parent's request) 
  setPushState(shouldBePushed: boolean) {
    if (this.props.usePushState) {
      this.setState({pushed: shouldBePushed});
    }
  }

  _onMouseEnter(e: SyntheticMouseEvent) {
    this.setState({hovered: true});
  }

  _onMouseLeave(e: SyntheticMouseEvent) {
    this.setState({hovered: false});
  }

  /*************************************************************************************************************************/
  // Styles and rendering

  getStyles(): any {
    return {
      hoverColor: Styles.Colors.pink300,
      pushedColor: Styles.Colors.green300,
      buttonStyle: {
        display: 'inline-block', // buttons should stack horizontally
        position: 'relative', 
        top: '50%', 
        transform: 'translateY(-50%)', 
        width: this.props.width, 
        minWidth: this.props.width, // needed to make override current minWidth, which prevents width from having an effect if too small
        height: this.props.height,
        marginLeft: this.props.spacing, // spacing between buttons
        // background color gets in the way of hoverColor and the default is already set by toolbarStyle
      },
      // Put an icon in the center of the button. If there's an arrow as well, the icon won't be centered horizontally in the middle. 
      iconStyle: { 
        position: 'absolute',
        top: '50%',
        left: this.props.includeDropdownArrow ? `calc(50% - ${this.props.arrowSize/2.0 + 'px'})` : '50%',
        transform: 'translate(-50%, -50%)',
        color: this.state.hovered ? Styles.Colors.grey50 : this.props.iconColor,
      },
      // An arrow, the left edge of which is arrowSize from the right border
      arrowStyle: {
        position: 'absolute',
        top: '50%',
        width: this.props.arrowSize,
        height: this.props.arrowSize,
        left: `calc(100% - ${this.props.arrowSize + 'px'})`,
        transform: 'translate(0%, -50%)'
      }
    };
  }

  render(): React.Element {
    let {hoverColor, pushedColor, buttonStyle, iconStyle, arrowStyle, arrowSepStyle} = this.getStyles();
    if (this.state.pushed) {
      buttonStyle.backgroundColor = pushedColor;
    }
    // If the user passes in their own icon element, use that, or use their material-ui icon, or default 
    let iconElement = (this.props.iconElement == null) ? 
      <FontIcon style={iconStyle} className="material-icons"> {this.props.iconName} </FontIcon>
      : this.props.iconElement;

    // Include an arrow if the user wanted to
    let arrowElement = null;
    if (this.props.includeDropdownArrow) {
      arrowElement = <DropDownArrow style={arrowStyle} />;
    }

    // Display the tooltip only if the user says so, and if the control isn't visible. 
    // Note that we use tooltip for the tooltipId, so this technically assumes (reasonably) that tooltips are different
    let buttonElement = (this.props.showTooltip) ? 
      <span>
        <FlatButton 
          data-for={this.props.tooltip}
          data-tip={this.props.tooltip}
          style={buttonStyle}
          onClick={this._onClick}
          onMouseLeave={this._onMouseLeave}
          onMouseEnter={this._onMouseEnter}>
          {iconElement}
          {arrowElement}
        </FlatButton>
        <Tooltip 
          id={this.props.tooltip}
          delayHide={50}
          delayShow={300}
          place="bottom"
          type="info"
          effect="solid"          
          offset={{'top': 10, 'left': 0}} />
      </span>
    : <FlatButton 
        style={buttonStyle}
        onClick={this._onClick}
        onMouseLeave={this._onMouseLeave}
        onMouseEnter={this._onMouseEnter}>
        {iconElement}
        {arrowElement}
      </FlatButton>;

    return buttonElement;
  }

  
}

ToolbarButton.propTypes = {
  width: React.PropTypes.number, 
  height: React.PropTypes.number,
  spacing: React.PropTypes.number, 
  onClick: React.PropTypes.func, 
  iconName: React.PropTypes.string, 
  iconElement: React.PropTypes.object, 
  tooltip: React.PropTypes.string, 
  usePushState: React.PropTypes.bool,
  showTooltip: React.PropTypes.bool,
  includeDropdownArrow: React.PropTypes.bool,
  arrowSize: React.PropTypes.number,
  iconColor: React.PropTypes.string
};

ToolbarButton.defaultProps = {
  width: 36,
  height: 36,
  spacing: 2,
  onClick: (e, state) => {},
  iconName: 'home',
  iconElement: null,
  usePushState: true,
  tooltip: "",
  showTooltip: true,
  includeDropdownArrow: false,
  arrowSize: 15,
  iconColor: Styles.Colors.grey500
};
