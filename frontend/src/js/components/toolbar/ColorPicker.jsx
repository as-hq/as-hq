// @flow

import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
// $FlowFixMe
import ColorPicker from 'react-color';

import ToolbarButton from './ToolbarButton.jsx';
import DropdownMenu from './DropdownMenu.jsx';

// TODO(joel) - this is unused
type ColorPickerProps = {
  tooltip: string;
  iconName: string;
  value: string;
  active: boolean;
  onOpen: () => void;
  onClose: () => void;
};

export default React.createClass({

  shouldComponentUpdate(nextProps: ColorPickerProps, nextState: any): boolean {
    // const x = shallowCompare(this, nextProps, nextState);
    // XXX(joel) onOpen, onClose ruin optimization
    // TODO(joel) make functional component
    return !(
         this.props.tooltip === nextProps.tooltip
      && this.props.iconName === nextProps.iconName
      && this.props.value === nextProps.value
      && this.props.active === nextProps.active
    );
  },

  propTypes: {
    tooltip: React.PropTypes.string.isRequired,
    iconName: React.PropTypes.string.isRequired,
    value: React.PropTypes.string.isRequired,
    active: React.PropTypes.bool.isRequired,
    onOpen: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired,
  },

  // Easy way to deal with closing menu upon clickaway
  // $FlowFixMe
  mixins: [require('react-clickaway')],

  _onButtonClick() {
    if (this.props.active) {
      this.props.onClose();
    } else {
      this.props.onOpen();
    }
  },

  _onPickerClose() {
    this.props.onClose();
  },

  // Close picker when user clicks away if picker is currently visible
  componentClickAway() {
    if (this.props.active) {
      this.props.onClose();
    }
  },

  render() {
    const {active, iconName, tooltip, onSelect, value} = this.props;

    const button = (
      <ToolbarButton
        spacing={7}
        width={43}
        showTooltip={!active}
        iconName={iconName}
        includeDropdownArrow
        tooltip={tooltip}
        onClick={() => this._onButtonClick()}
      />
    );

    const colorPicker = active
      ? (
        <div style={styles.colorPickerStyle}>
          <ColorPicker
            onChangeComplete={onSelect}
            color={value}
            position="top"
            display="top"
            type="compact"
          />
        </div>
      )
      : <noscript />;

    return (
      <DropdownMenu
        toolbarWidth={36}
        toolbarComponent={button}
        menuComponent={colorPicker}
      />
    );
  },
});

const styles = {
  colorPickerStyle: {
    position: 'absolute',
    width: 245,
  },
};
