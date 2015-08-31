import React, {PropTypes} from 'react';
import {
  AppCanvas,
  EnhancedButton,
  Paper,
  Styles,
  Mixins,
  Utils
} from 'material-ui';

let {Colors, Transitions, Typography} = Styles;
let {StylePropable} = Mixins;
let {ColorManipulator} = Utils;

function validateLabel (props, propName, componentName) {
  if (!props.children && !props.labelElement && !props.label) {
    return new Error('Required prop label or children was not ' +
      'specified in ' + componentName + '.');
  }
}

export default React.createClass({
  mixins: [StylePropable],

  contextTypes: {
    muiTheme: React.PropTypes.object
  },

  propTypes: {
    className: React.PropTypes.string,
    disabled: React.PropTypes.bool,
    label: validateLabel,
    onMouseDown: React.PropTypes.func,
    onMouseUp: React.PropTypes.func,
    onMouseLeave: React.PropTypes.func,
    onTouchEnd: React.PropTypes.func,
    onTouchStart: React.PropTypes.func,
    primary: React.PropTypes.bool,
    secondary: React.PropTypes.bool,
    labelStyle: React.PropTypes.object,
    backgroundColor: React.PropTypes.string,
    labelColor: React.PropTypes.string,
    disabledBackgroundColor: React.PropTypes.string,
    disabledLabelColor: React.PropTypes.string,
    fullWidth: React.PropTypes.bool,
    selectable: React.PropTypes.bool,
    defaultPushedIn: React.PropTypes.bool
  },

  getInitialState() {
    let zDepth = this.props.disabled ? 0 : 1;
    return {
      hovered: false,
      touched: false,
      pushedIn: this.props.defaultPushedIn,
      initialZDepth: zDepth,
      zDepth: zDepth
    };
  },

  componentWillReceiveProps(nextProps) {
    let zDepth = nextProps.disabled ? 0 : 1;
    this.setState({
      zDepth: zDepth,
      initialZDepth: zDepth
    });
  },

  _getBackgroundColor() {
    let disabledColor = this.props.disabledBackgroundColor ? this.props.disabledBackgroundColor :
      this.getTheme().disabledColor;

    return this.props.disabled ? disabledColor :
      this.state.pushedIn ? Colors.cyan700 :
      this.props.backgroundColor ? this.props.backgroundColor :
      this.props.primary ? this.getTheme().primaryColor :
      this.props.secondary ? this.getTheme().secondaryColor :
      this.getTheme().color;
  },

  _getLabelColor() {
    let disabledColor = this.props.disabledLabelColor ? this.props.disabledLabelColor :
      this.getTheme().disabledTextColor;

    return this.props.disabled ? disabledColor :
      this.props.labelColor ? this.props.labelColor :
      this.props.primary ? this.getTheme().primaryTextColor :
      this.props.secondary ? this.getTheme().secondaryTextColor :
      this.getTheme().textColor;
  },

  getThemeButton() {
    return this.context.muiTheme.component.button;
  },

  getTheme() {
    return this.context.muiTheme.component.raisedButton;
  },

  getStyles() {
    let amount = (this.props.primary || this.props.secondary) ? 0.4 : 0.08;
    let styles = {
      root: {
        backgroundColor: 'none',
        display: 'inline-block',
        minWidth: this.props.fullWidth ? '100%' : this.getThemeButton().minWidth,
        height: this.getThemeButton().height,
        transition: Transitions.easeOut()
      },
      container: {
        position: 'relative',
        height: '100%',
        width: '100%',
        padding: '0',
        overflow: 'hidden',
        borderRadius: 2,
        transition: Transitions.easeOut(),
        backgroundColor: this._getBackgroundColor(),

        //This is need so that ripples do not bleed
        //past border radius.
        //See: http://stackoverflow.com/questions/17298739/css-overflow-hidden-not-working-in-chrome-when-parent-has-border-radius-and-chil
        transform: 'translate3d(0, 0, 0)'
      },
      label: {
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
      },
      labelElement: {
        position: 'relative',
        opacity: 1,
        margin: 0,
        padding: '0px ' + this.context.muiTheme.spacing.desktopGutterLess + 'px',
        fontSize: '14px',
        letterSpacing: 0,
        textTransform: 'uppercase',
        fontWeight: Typography.fontWeightMedium,
        color: this._getLabelColor(),
        userSelect: 'none'
      },
      overlay: {
        transition: Transitions.easeOut(),
        top: 0
      },
      overlayWhenHovered: {
        backgroundColor: ColorManipulator.fade(this._getLabelColor(), amount)
      }
    };
    return styles;
  },

  render() {
    let {
      disabled,
      label,
      labelElement,
      primary,
      secondary,
      selectable,
      defaultPushedIn,
      ...other } = this.props;

    let styles = this.getStyles();

    let finalizedLabelElement;
    if (labelElement) {
      finalizedLabelElement = React.cloneElement(labelElement, {
        style: this.mergeAndPrefix(styles.labelElement, this.props.labelStyle)
      });
    } else if (label) {
      finalizedLabelElement = (
        <span style={this.mergeAndPrefix(styles.label, this.props.labelStyle)}>
          {label}
        </span>
      );
    }

    let rippleColor = styles.label.color;
    let rippleOpacity = !(primary || secondary) ? 0.1 : 0.16;

    let buttonEventHandlers = disabled ? null : {
      onMouseDown: this._handleMouseDown,
      onMouseUp: this._handleMouseUp,
      onMouseLeave: this._handleMouseLeave,
      onMouseEnter: this._handleMouseEnter,
      onTouchStart: this._handleTouchStart,
      onTouchEnd: this._handleTouchEnd,
      onKeyboardFocus: this._handleKeyboardFocus
    };

    return (
      <Paper
        style={this.mergeAndPrefix(styles.root, this.props.style)}
        zDepth={this.state.zDepth}>
          <EnhancedButton
            {...other}
            {...buttonEventHandlers}
            ref="container"
            disabled={disabled}
            style={this.mergeAndPrefix(styles.container)}
            focusRippleColor={rippleColor}
            touchRippleColor={rippleColor}
            focusRippleOpacity={rippleOpacity}
            touchRippleOpacity={rippleOpacity}>
              <div ref="overlay" style={this.mergeAndPrefix(
                  styles.overlay,
                  (this.state.hovered && !this.props.disabled) && styles.overlayWhenHovered
                )}>
                  {finalizedLabelElement}
                  {this.props.children}
              </div>
          </EnhancedButton>
      </Paper>
    );
  },

  _handleMouseDown(e) {
    //only listen to left clicks
    if (e.button === 0) {
      this.setState({ zDepth: this.state.initialZDepth + 1 });
    }
    if (this.props.onMouseDown) this.props.onMouseDown(e);
  },

  _handleMouseUp(e) {
    this.setState({ zDepth: this.state.initialZDepth });
    if (this.props.selectable) {
      this.setState({ pushedIn: !this.state.pushedIn });
    }

    if (this.props.onMouseUp) this.props.onMouseUp(e);
  },

  _handleMouseLeave(e) {
    if (!this.refs.container.isKeyboardFocused()) this.setState({ zDepth: this.state.initialZDepth, hovered: false });
    if (this.props.onMouseLeave) this.props.onMouseLeave(e);
  },

  _handleMouseEnter(e) {
    if (!this.refs.container.isKeyboardFocused() && !this.state.touch) {
      this.setState({hovered: true});
    }
    if (this.props.onMouseEnter) this.props.onMouseEnter(e);
  },

  _handleTouchStart(e) {
    this.setState({
      touch: true,
      zDepth: this.state.initialZDepth + 1,
    });
    if (this.props.onTouchStart) this.props.onTouchStart(e);
  },

  _handleTouchEnd(e) {
    this.setState({ zDepth: this.state.initialZDepth });
    if (this.props.onTouchEnd) this.props.onTouchEnd(e);
  },

  _handleKeyboardFocus(e, keyboardFocused) {
    if (keyboardFocused && !this.props.disabled) {
      this.setState({ zDepth: this.state.initialZDepth + 1 });
      let amount = (this.props.primary || this.props.secondary) ? 0.4 : 0.08;
      React.findDOMNode(this.refs.overlay).style.backgroundColor = ColorManipulator.fade(this.mergeAndPrefix(this.getStyles().label, this.props.labelStyle).color, amount);
    }
    else if (!this.state.hovered) {
      this.setState({ zDepth: this.state.initialZDepth });
      React.findDOMNode(this.refs.overlay).style.backgroundColor = 'transparent';
    }
  }
});
