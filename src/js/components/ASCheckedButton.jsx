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
    iconClassName: React.PropTypes.string.isRequired,
    label: React.PropTypes.string,
    defaultPushedIn: React.PropTypes.bool,
    onCheckChange: React.PropTypes.func.isRequired
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
    let {width, height, iconClassName, label, defaultPushedIn} = this.props;

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
      <ASButton
        ref="button"
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
            { label ?
              <span style={labelElementStyle}>
                {label}
              </span> : null
            }
          </div>
        }
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
