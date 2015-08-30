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
    iconClassName: React.PropTypes.string,
    label: React.PropTypes.string,
    defaultPushedIn: React.PropTypes.bool,
    onCheckChange: React.PropTypes.func.isRequired, 
    iconImageSource: React.PropTypes.string, 
  },

  getInitialState() {
    return {
      hover: false
    };
  },

  getDefaultProps() {
    return {
      height: '24px',
      defaultPushedIn: false
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
    let {width, height, iconClassName, label, defaultPushedIn, iconImageSource} = this.props;

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
    if (iconClassName) { 
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
    } else if (iconImageSource) { 
      labelElement = (
        <span style={labelElementStyle}>
          <img src={iconImageSource} width="18" height="24" />
        </span>
      );
    }

    //__TODO__ #alex proposal: pass in an object that is either a font icon or an image. then do casework based on what that thing passed in is

    return (
      <ASButton
        ref="button"
        labelElement={labelElement}
        labelStyle={{
          padding: '0px 7px'
        }}
        style={{
          width: width,
          minWidth: width,
          height: height
        }}
        backgroundColor={ this.state.hover ? Colors.cyan400 : Colors.grey800 }
        selectable={true}
        defaultPushedIn={defaultPushedIn}
        onMouseEnter={this._onMouseEnter}
        onMouseLeave={this._onMouseLeave}
        onMouseUp={this._onMouseUp}
      />
    );
  },

  _onMouseEnter() {
    this.setState({ hover: true });
  },

  _onMouseLeave() {
    this.setState({ hover: false });
  },

  _onMouseUp() {
    this.props.onCheckChange(this.refs.button.state.pushedIn);
  }
});
