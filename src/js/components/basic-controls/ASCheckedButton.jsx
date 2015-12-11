import React, {PropTypes} from 'react';
import ASButton from './ASButton.jsx';
import {AppCanvas, FontIcon, Styles} from 'material-ui';

let {Colors, Typography} = Styles;

export default React.createClass({
  contextTypes: {
    muiTheme: React.PropTypes.object
  },

  propTypes: {
    width: React.PropTypes.string,
    height: React.PropTypes.string,
    checked: React.PropTypes.bool.isRequired,
    iconClassName: React.PropTypes.string,
    label: React.PropTypes.string,
    onCheckChange: React.PropTypes.func.isRequired,
    iconImageSource: React.PropTypes.string,
  },

  getDefaultProps() {
    return {
      width: '32px',
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
    let {width, height, checked, iconClassName, label, iconImageSource, style, ...etc} = this.props;

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

    let labelElement;
    labelElement = (
      <div style={{ display: 'inline' }}>
        <FontIcon
          style={{
            float: 'left',
            fontSize: '18px',
            lineHeight: '24px'
          }}
          className="material-icons"
          color={Colors.grey50}
        >
          {iconClassName}
        </FontIcon>
        { label ?
          <span style={labelElementStyle}>
            {label}
          </span> : null
        }
      </div>);

    if (iconImageSource) {
      labelElement = (
        <span style={labelElementStyle}>
          <img src={iconImageSource} width="18" height="24" />
        </span>
      );
    }

    return (
      <ASButton
        ref="button"
        primary={checked}
        labelElement={labelElement}
        labelStyle={{
          padding: '0px 7px'
        }}
        style={{
          ...style,
          width: width,
          minWidth: width,
          height: height
        }}
        selectable={false}
        {...etc}
      />
    );
  }
});
