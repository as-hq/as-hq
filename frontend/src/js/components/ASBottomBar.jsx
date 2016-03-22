import React from 'react';
import {Paper} from 'material-ui';

import U from '../AS/Util';

// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';
// $FlowFixMe
import FontIcon from 'material-ui/lib/font-icon';

import {bottomBar as bottomBarZIndex} from '../styles/zIndex';

import _ from 'lodash';

type Props = {
  errorIconStyle: any;
  outputIconStyle: any;
  sheetName: string;
};

function LabeledIconButton(props: {
  label: string;
  onClick: () => void;
  contentStyle: any;
  tooltip: string;
  iconClassName: string;
}): React.Element {
  const {label, onClick, contentStyle, tooltip, iconClassName} = props;

  return <div style={styles.button}>
    <IconButton
      style={styles.iconButton}
      iconStyle={contentStyle}
      onClick={onClick}
      iconClassName="material-icons"
      tooltip={tooltip}
      tooltipPosition="top-right"
      tooltipStyles={styles.tooltip} >
      {iconClassName}
    </IconButton>
    <div
      style={styles.buttonLabel}
      onClick={onClick} >
      {label}
    </div>
  </div>;
}

export default class ASBottomBar extends React.Component<{}, Props, {}>
{
  constructor(props: Props) {
    super(props);
  }

  shouldComponentUpdate(nextProps: Props, nextState: {}): boolean {
    // have to check this manually because we can't compare functions (which get passed as props)
    return !(_.isEqual(nextProps.errorIconStyle, this.props.errorIconStyle) &&
             _.isEqual(nextProps.outputIconStyle, this.props.outputIconStyle) &&
             nextProps.sheetName === this.props.sheetName);
  }

  render(): React.Element {
    const {errorIconStyle, outputIconStyle, sheetName,
           onErrorIconClick, onOutputIconClick, onHeaderIconClick} = this.props;

    return (
      <Paper style={styles.root}>
        <LabeledIconButton
          contentStyle={errorIconStyle}
          onClick={onErrorIconClick}
          tooltip={`Errors (${U.Browser.metaKeyName()}+Alt+E)`}
          iconClassName="error_outline"
          label="Errors"
        />

        <LabeledIconButton
          contentStyle={outputIconStyle}
          onClick={onOutputIconClick}
          tooltip={`Cell output (${U.Browser.metaKeyName()}+Alt+O)`}
          iconClassName="label_outline"
          label="Cell output"
        />

        <LabeledIconButton
          onClick={onHeaderIconClick}
          tooltip={`Header output (${U.Browser.metaKeyName()}+Alt+H)`}
          iconClassName="input"
          label="Header output"
        />

        <span style={styles.sheetName(sheetName)}>
          { sheetName }
        </span>

      </Paper>
    );
  }
}

// TODO flex this shit
const styles = {
  root: {
    position: 'relative',
    display: 'block',
    height: '24px',
    background: '#212121',
    zIndex: bottomBarZIndex,
  },

  button: {
    position: 'relative',
    display: 'inline-block',
    top: '50%',
    transform: 'translateY(-50%)' // vertically center
  },

  iconButton: {
    display: 'inline-block',
    width: '40px'
  },

  buttonLabel: {
    position: 'relative',
    display: 'inline-block',
    color: '#ffffff',
    fontWeight: 500,
    top: '-7px'
  },

  tooltip: {
    top: 0,
    zIndex: 1000 // to be visible on top of spreadsheet when closed
  },

  sheetName: (name) => { return {
    position: 'absolute',
    right: `calc(50% - ${name.length * 4}px)`,
    top: 3,
    width: 'auto',
    color: 'white',
    fontWeight: 'bold',
  }}
};
