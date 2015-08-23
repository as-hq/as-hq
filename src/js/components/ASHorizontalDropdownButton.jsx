import React, {PropTypes} from 'react';
import ASDropdownButton from './ASDropdownButton.jsx';
import {AppCanvas, FontIcon, Styles} from 'material-ui';

let {Colors, Typography} = Styles;

export default React.createClass({
  contextTypes: {
    muiTheme: React.PropTypes.object
  },

  getDefaultProps() {
    return {
    };
  },

  getTheme() {
    console.log('outer theme:', JSON.stringify(this.context.muiTheme));
    console.log('theme:', this.context.muiTheme.component.raisedButton);
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
      fontSize: '14px',
      letterSpacing: 0,
      textTransform: 'uppercase',
      fontWeight: Typography.fontWeightMedium,
      margin: 0,
      padding: '0px ' + this.context.muiTheme.spacing.desktopGutterLess + 'px',
      userSelect: 'none',
      lineHeight: (this.props.style && this.props.style.height) ?
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
                fontSize: '24px',
                lineHeight: '36px'
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
                fontSize: '24px',
                lineHeight: '36px'
              }}
              className="muidocs-icon-navigation-expand-more"
              color={Colors.grey50}
            />
          </div>
        }
        width={width}
        height={height}
        menuItems={menuItems}
      />
    );
  }
});
