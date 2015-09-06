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
    let {primary, width, height, iconClassName, label, menuItems, style, buttonStyle, onItemClick} = this.props;

    let lineHeight = (this.props.height) ? this.props.height :
      (this.props.style && this.props.style.height) ?
      this.props.style.height : this.getThemeButton().height + 'px';

    let labelElementStyle = {
      position: 'relative',
      opacity: 1,
      fontSize: (buttonStyle && buttonStyle.fontSize) ? buttonStyle.fontSize : '10px',
      letterSpacing: 0,
      textTransform: 'uppercase',
      fontWeight: Typography.fontWeightMedium,
      margin: 0,
      padding: '0px 7px',
      userSelect: 'none',
      lineHeight: lineHeight,
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
                lineHeight: lineHeight
              }}
              className={iconClassName}
              color={Colors.grey50}
            />
            { label ?
              <span style={labelElementStyle}>
                {label}
              </span> : null
            }
            <FontIcon
              style={{
                float: 'right',
                fontSize: '18px',
                lineHeight: lineHeight
              }}
              className="muidocs-icon-navigation-expand-more"
              color={Colors.grey50}
            />
          </div>
        }
        primary={primary}
        labelStyle={{
          padding: '0px 7px'
        }}
        style={style}
        buttonStyle={buttonStyle}
        width={width}
        height={height}
        menuItems={menuItems}
        onItemClick={onItemClick}
      />
    );
  }
});
