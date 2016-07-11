/* @flow */

import type {
  Callback
} from '../types/Base';

import type { StoreToken } from 'flux';

import React from 'react';
// $FlowFixMe:
import {AppBar, IconButton, RaisedButton, List, ListItem} from 'material-ui';
// $FlowFixMe:
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe:
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';
// $FlowFixMe:
import NavigationClose from 'material-ui/lib/svg-icons/navigation/close';
// $FlowFixMe:
import ActionAssignment from 'material-ui/lib/svg-icons/action/assignment';

import U from '../AS/Util';
import ConfigActions from '../actions/ASConfigActionCreators';
import API from '../actions/ASApiActionCreators';
import CheckpointStore from '../stores/ASCheckpointStore';

type Props = {
};

// The name refers to the fileName of the currently selected revision, which 
// is the checkpointTime, and the user refers to the checkpointUser.
type State = {
  curName: string;
  curUser: string;
};

class CheckpointViewer extends React.Component {
  static defaultProps = {};
  props: Props;
  state: State;
  _storeToken: StoreToken;

  constructor(props: Props) {
    super(props);
    this.state = {
      curName: '',
      curUser: ''
    }
  }

  componentDidMount() {
    this._storeToken = CheckpointStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._storeToken.remove();
  }

  getChildContext(): any {
    return {
      muiTheme: ThemeManager.getMuiTheme(LightTheme)
    };
  }

  handleClose() {
    API.revertCheckpoint();
    ConfigActions.toggleCheckpointView();
  }

  onClick(name: string,  user: string) {
    API.viewCheckpoint(name, user);
    this.setState({curName: name, curUser: user});
  }

  applyRevision(name: string, user: string) {
    API.applyCheckpoint(name, user);
    this.handleClose();
  }

  render(): React.Element {
    const checkpoints = CheckpointStore.getAllCheckpoints().toArray();
    const getStyle = (cp) => cp.checkpointTime === this.state.curName ? 
      {backgroundColor: 'orange', color: 'black'} : {color: 'white'};
    const items = checkpoints.map((cp) => 
        <ListItem
          leftAvatar={<ActionAssignment />}
          style={getStyle(cp)}
          primaryText={cp.checkpointDesc}
          secondaryText={
            <div>
              <div style={getStyle(cp)}> {cp.checkpointUser} </div>
              <div style={{height: '2px'}} />
              <div style={getStyle(cp)}> {cp.checkpointTime} </div>
            </div>
          }
          secondaryTextLines={2}
          key={cp.name} 
          onTouchTap={(e) => this.onClick(cp.checkpointTime, cp.checkpointUser)}
        />
    );

    return (
      <div style={{overflow: 'auto'}}>
        <AppBar
            title="Revision history"
            titleStyle={{fontSize: '18px'}}
            style={{backgroundColor: 'black'}}
            iconElementLeft={null}
            iconElementRight={
              <IconButton onClick={(e) => this.handleClose()}>
                <NavigationClose />
              </IconButton>}
          />
        <RaisedButton
          style={{
            marginTop: '20px', 
            width: '90%', 
            marginLeft: '5%'}}
          label="Apply selected revision"
          onTouchTap={() => 
            this.applyRevision(this.state.curName, this.state.curUser)
          } />
        <div style={{marginTop: '20px'}} />
        {items}
      </div>
    );
  }

}

CheckpointViewer.childContextTypes = {
  muiTheme: React.PropTypes.object
};

export default CheckpointViewer;
