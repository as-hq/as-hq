import React, {PropTypes} from 'react';
import ASDropdownButton from './ASDropdownButton.jsx';
import {AppCanvas, FontIcon, Styles} from 'material-ui';

let {Colors, Typography} = Styles;

export default React.createClass({
  contextTypes: {
    muiTheme: React.PropTypes.object
  },

  propTypes: {
    width: React.PropTypes.string,
    height: React.PropTypes.string,
    iconClassName: React.PropTypes.string.isRequired,
    label: React.PropTypes.string.isRequired,
    menuItems: React.PropTypes.arrayOf(React.PropTypes.string).isRequired
  },

  getDefaultProps() {
    return {
      height: '24px'
    };
  },

  getTheme() {
    return this.context.muiTheme.component.raisedButton;
  },

  getThemeButton() {
    return this.context.muiTheme.component.button;
  },

  _getLabelColor() {
    return this.getTheme().textColor;
  },

  render() {
    let {width, height, iconClassName, label, menuItems} = this.props;

    let labelElementStyle = {
      position: 'relative',
      opacity: 1,
      fontSize: '10px',
      letterSpacing: 0,
      textTransform: 'uppercase',
      fontWeight: Typography.fontWeightMedium,
      margin: 0,
      padding: '0px 7px',
      userSelect: 'none',
      lineHeight:
        (this.props.height) ? this.props.height :
        (this.props.style && this.props.style.height) ?
        this.props.style.height : this.getThemeButton().height + 'px',
      color:  this._getLabelColor()
    };

    return (
      <ASDropdownButton
        labelElement={
          <div style={{ display: 'inline' }}>
            <FontIcon
              style={{
                float: 'left',
                fontSize: '18px',
                lineHeight: '24px'
              }}
              className={iconClassName}
              color={Colors.grey50}
            />
            <span style={labelElementStyle}>
              {label}
            </span>
            <FontIcon
              style={{
                float: 'right',
                fontSize: '18px',
                lineHeight: '24px'
              }}
              className="muidocs-icon-navigation-expand-more"
              color={Colors.grey50}
            />
          </div>
        }
        labelStyle={{
          padding: '0px 7px'
        }}
        width={width}
        height={height}
        menuItems={menuItems}
      />
    );
  }
});
