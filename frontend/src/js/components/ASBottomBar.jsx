import React from 'react';
import {Paper} from 'material-ui';

import U from '../AS/Util';

// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';
// $FlowFixMe
import FontIcon from 'material-ui/lib/font-icon';

import {bottomBar as bottomBarZIndex} from '../styles/zIndex';

import SheetStateStore from '../stores/ASSheetStateStore';

type Props = {
  errorIconStyle: any; 
  outputIconStyle: any; 
};

export default class ASErrorPane extends React.Component<{}, Props, {}>
{
  constructor(props: Props) {
    super(props);
  }

  shouldComponentUpdate(nextProps: Props, nextState: {}): boolean { 
    return !_.isEqual(nextProps, this.props); 
  }

  render(): React.Element {
    const {errorIconStyle, outputIconStyle, 
           onErrorIconClick, onOutputIconClick, onHeaderIconClick} = this.props; 
    return (
      <Paper style={styles.root}>
        <IconButton
          style={styles.button}
          iconStyle={errorIconStyle}
          onClick={onErrorIconClick}
          iconClassName="material-icons"
          tooltip={`Errors (${U.Browser.metaKeyName()}+Alt+E)`}
          tooltipPosition="top-right"
          tooltipStyles={styles.tooltip} >
          error_outline
        </IconButton>

        <IconButton
          style={styles.button}
          iconStyle={outputIconStyle}
          onClick={onOutputIconClick}
          iconClassName="material-icons"
          tooltip={`Cell output (${U.Browser.metaKeyName()}+Alt+O)`}
          tooltipPosition="top-right"
          tooltipStyles={styles.tooltip}>
          label_outline
        </IconButton>

        <IconButton
          style={styles.button}
          onClick={onHeaderIconClick}
          iconClassName="material-icons"
          tooltip={`Header output (${U.Browser.metaKeyName()}+Alt+H)`}
          tooltipPosition="top-right"
          tooltipStyles={styles.tooltip}>
          input
        </IconButton>

        <span style={styles.sheetName}>
          { SheetStateStore.getCurrentSheetName() }
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
    width: '40px',
    top: '50%',
    transform: 'translateY(-50%)' // vertically center
  },

  tooltip: {
    top: 0,
    zIndex: 1000 // to be visible on top of spreadsheet when closed
  },

  sheetName: {
    position: 'absolute',
    right: 10,
    top: 3,
    width: 'auto',
    color: 'white',
    fontWeight: 'bold',
  }
};
