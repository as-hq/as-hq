// @flow
//
// A button with an icon and a tooltip

import React from 'react';

import {Styles, FontIcon} from 'material-ui';
// $FlowFixMe
const FlatButton = require('material-ui/lib/flat-button');
// $FlowFixMe
import DropDownArrow from 'material-ui/lib/svg-icons/navigation/arrow-drop-down';
import Tooltip from 'react-tooltip';


type ToolbarButtonDefaultProps = {
  width: number;
  height: number;
  // left margin
  spacing: number;
  onClick: () => void;
  // iconName: name of the material-ui icon used for the FontIcon (print, home,
  // etc)
  iconName: string;
  // HTML Element of the icon itself can be passed; overrides iconName if not
  // null
  iconElement: ?React.Element;
  // actual message underneath button (ex. Print (Ctrl+P))
  tooltip: string;
  showTooltip: boolean;
  // Should we include a dropdown arrow inside the button? This wouldn't do
  // anything by itself if clicked.
  includeDropdownArrow: boolean;
  arrowSize: number;
  iconColor: string;
};

type ToolbarButtonProps = {
  width: number;
  height: number;
  spacing: number;
  onClick: () => void; // #needsrefactor only any because type of second arg depends on a prop...
  iconName: string;
  iconElement: ?React.Element;
  tooltip: string;
  showTooltip: boolean;
  includeDropdownArrow: boolean;
  arrowSize: number;
  iconColor: string;
  active?: boolean;
};

export default class ToolbarButton
  extends React.Component<ToolbarButtonDefaultProps, ToolbarButtonProps, {}> {

  shouldComponentUpdate(nextProps: ToolbarButtonProps): boolean {
    // XXX(joel) onClick is created fresh each time, ruins optimization
    // TODO(joel) make functional component, use onlyUpdateForKeys
    return !(
         this.props.width === nextProps.width
      && this.props.height === nextProps.height
      && this.props.spacing === nextProps.spacing
      && this.props.iconName === nextProps.iconName
      && this.props.iconElement === nextProps.iconElement
      && this.props.tooltip === nextProps.tooltip
      && this.props.showTooltip === nextProps.showTooltip
      && this.props.includeDropdownArrow === nextProps.includeDropdownArrow
      && this.props.arrowSize === nextProps.arrowSize
      && this.props.iconColor === nextProps.iconColor
      && this.props.active === nextProps.active
    );
  }

  getStyles(): any {
    const {
      width,
      height,
      spacing,
      includeDropdownArrow,
      iconColor,
      activeBackgroundColor,
      arrowSize,
      active
    } = this.props;

    return {
      buttonStyle: {
        display: 'inline-block', // buttons should stack horizontally
        position: 'relative',
        top: '50%',
        transform: 'translateY(-50%)',
        width,
        minWidth: width, // needed to make override current minWidth, which prevents width from having an effect if too small
        height,
        marginLeft: spacing, // spacing between buttons
        // background color gets in the way of hoverColor and the default is already set by toolbarStyle
        ...(active ? { backgroundColor: activeBackgroundColor } : {})
      },
      // Put an icon in the center of the button. If there's an arrow as well, the icon won't be centered horizontally in the middle.
      iconStyle: {
        position: 'absolute',
        top: '50%',
        left: includeDropdownArrow ? `calc(50% - ${this.props.arrowSize / 2.0 + 'px'})` : '50%',
        transform: 'translate(-50%, -50%)',
        color: iconColor,
      },
      // An arrow, the left edge of which is arrowSize from the right border
      arrowStyle: {
        position: 'absolute',
        top: '50%',
        width: arrowSize,
        height: arrowSize,
        left: `calc(100% - ${this.props.arrowSize + 'px'})`,
        transform: 'translate(0%, -50%)',
      },
    };
  }

  render(): React.Element {
    const {buttonStyle, iconStyle, arrowStyle} = this.getStyles();

    // If the user passes in their own icon element, use that, or use their material-ui icon, or default
    const iconElement = this.props.iconElement == null
      ? (
        <FontIcon
          // TODO(joel): could this be a <GenIcon> ?
          style={iconStyle}
          className="material-icons toolbar-button-icon"
        >
          {this.props.iconName}
        </FontIcon>
      )
      : this.props.iconElement;

    // Include an arrow if the user wanted to
    let arrowElement = null;
    if (this.props.includeDropdownArrow) {
      arrowElement = <DropDownArrow style={arrowStyle} />;
    }

    // Display the tooltip only if the user says so, and if the control isn't
    // visible.
    //
    // Note that we use tooltip for the tooltipId, so this technically assumes
    // (reasonably) that tooltips are different
    const button = (
      <FlatButton
        data-for={this.props.tooltip}
        data-tip={this.props.tooltip}
        style={buttonStyle}
        onClick={() => this.props.onClick()}
      >
        {iconElement}
        {arrowElement}
      </FlatButton>
    );

    const tip = (
      <Tooltip
        id={this.props.tooltip}
        delayHide={50}
        delayShow={300}
        place="bottom"
        type="info"
        effect="solid"
        offset={{'top': 10, 'left': 0}}
      />
    );

    return (
      <span>
        {button}
        {this.props.showTooltip && tip}
      </span>
    );
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
  showTooltip: React.PropTypes.bool,
  includeDropdownArrow: React.PropTypes.bool,
  arrowSize: React.PropTypes.number,
  iconColor: React.PropTypes.string,
  activeBackgroundColor: React.PropTypes.string,
};

ToolbarButton.defaultProps = {
  width: 36,
  height: 36,
  spacing: 2,
  onClick: () => {},
  iconName: 'home',
  iconElement: null,
  tooltip: '',
  showTooltip: true,
  includeDropdownArrow: false,
  arrowSize: 15,
  iconColor: Styles.Colors.grey500,
  activeBackgroundColor: Styles.Colors.pink300,
};
