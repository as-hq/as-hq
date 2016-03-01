import React from 'react';
import { Card, CardText, CardActions, CardHeader, Styles, List, ListItem, Avatar, FlatButton, RaisedButton, TextField } from 'material-ui';
const { Colors } = Styles;
// $FlowFixMe
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';
// $FlowFixMe
import Checkbox from 'material-ui/lib/checkbox';

import LogStore from '../stores/ASLogStore';
import U from '../AS/Util';
import API from '../actions/ASApiActionCreators';
import Dispatcher from '../Dispatcher';

export default class LogViewer extends React.Component<{}, {}, {}> {

  constructor(props) {
    super(props);
    this._listener = () => this.forceUpdate();
    this.state = {
      displayAllSessions: true,
      onlyAPICalls: false
    };
  }
 
  // When this component is in the DOM, tell backend to stop logging actions/messages
  componentDidMount() {
    LogStore.addListener(this._listener);
    API.startDebuggingLog();
    API.getAllSessions();
  }

  componentWillUnmount() {
    LogStore.removeListener(this._listener);
  }

  getChildContext() {
    return {
      muiTheme: ThemeManager.getMuiTheme(LightTheme)
    };
  }

  // Tell all stores to reset themselves and clear the current sheet
  // Called before replaying 
  // #needs refactor overlap between reset and clearsheet
  _reset() {
    // TODO: this is asynchronous, and we may want to wait for it to get back before proceeding
    API.clearSheet();
    Dispatcher.dispatch({_type: 'RESET'});
  }

  // To replay until a given point, just dispatch actions or send API messages until we get to that point
  // We use API.sendMsg which doesn't have another JSON.stringify
  _replayUntil(index: number) {
    this._reset();
    for (var i = 0; i <= index; i++) {
      const log = this._getDisplayedLogs().get(i);
      if (log.isAction) {
        const action = JSON.parse(log.logMsg);
        console.log('Dispatching debug action:', action);
        Dispatcher.dispatch(action);
      } else {
        console.log('Sending API debug message:', JSON.parse(log.logMsg));
        API.sendMsg(log.logMsg);
      }
    }
  }

  _replayEntire() {
    const size = this._getDisplayedLogs().size;
    this._replayUntil(size - 1);
  }

  // When we select a session from the list of all sessions, get the logs for that sessions
  _onSessionSelect(userId: string, seshId: string) {
    API.getSessionLogs(userId, seshId);
    this.setState({displayAllSessions: false});
  }

  // When the user wants to see all of the sessions again, get the sessions and display them
  _onDisplayAllSessions() {
    API.getAllSessions();
    this.setState({displayAllSessions: true});
  }

  // Short description of this particular log
  _getDescription(logData: LogData) {
    if (logData.isAction) {
      const action = JSON.parse(logData.logMsg);
      return "Action: " + action._type;
    } else {
      const tag = JSON.parse(logData.logMsg).serverAction.tag;
      return "API call: " + tag;
    }
  }

  _toggleOnlyAPICalls() {
    this.setState({onlyAPICalls: !this.state.onlyAPICalls});
  }

  _getDisplayedLogs() {
    const allLogs = LogStore.getSessionLogs();
    return this.state.onlyAPICalls ? allLogs.filter((log) => !log.isAction) : allLogs;
  }

  _getBackgroundColor(ld: LogData) {
    return ld.isAction ? Colors.yellow600 : Colors.blue600;
  }

  renderSessionInfo() {
    const sessions = LogStore.getAllSessions();
    const [...users] = sessions.keys();
    return (
      <div style={{marginLeft: '10%', width: '80%', height: '50%', overflow: 'auto'}} >
        {
          users.map((user) => {
            return (
              <Card style={{width: '100%', marginTop: '5px'}}>
                <CardHeader 
                  title={user} 
                  subtitle={"User"}
                  style={{border: '2px solid black'}}
                  avatar="http://lorempixel.com/100/100/nature/"
                  actAsExpander={true}
                  showExpandableButton={true} />
                <CardText expandable={true} style={{border: '2px solid black'}} >
                  <List>
                    {
                      sessions.get(user).map((session, index) => {
                        return (
                          <ListItem 
                            key={index} 
                            leftAvatar={<Avatar backgroundColor={Colors.yellow600}>A</Avatar>} 
                            secondaryText={session.seshId}
                            onTouchTap={() => this._onSessionSelect(user, session.seshId)}>
                            {session.seshTime}
                          </ListItem>
                        );
                      })
                    }
                  </List>
                </CardText>
              </Card>
            );
          })
        }
      </div>
    );
  }

  renderSessionLogs() {
    const logs = this._getDisplayedLogs();
    const goBackButton = 
      <RaisedButton 
        label="View all sessions again" 
        style={{marginTop: '5px', marginLeft: '37%', width: '25%'}}
        onTouchTap={() => this._onDisplayAllSessions()} />;
    const flags = logs.size > 0 ? 
      [<RaisedButton 
        label="Replay Entire Session" 
        style={{marginTop: '5px', marginLeft: '37%', width: '25%'}}
        onTouchTap={() => this._replayEntire()} />,
      <Checkbox 
        label="Only display API calls to Haskell backend" 
        style={{marginTop: '5px', marginLeft: '37%', width: '25%'}}
        checked={this.state.onlyAPICalls} 
        onCheck={() => this._toggleOnlyAPICalls()} />,
      goBackButton] : [goBackButton];
    return (
      <div style={{width: '100%', height: '100%'}}>
        <Card style={{overflow: 'auto', height: '50%'}} >
          {flags}
          <CardText style={{paddingTop: '0px', paddingBottom: '0px', paddingLeft: '0px'}}>
            <List insetSubheader={false}>
              {
                logs.map((ld, index) => {
                  return (
                    <ListItem 
                      key={index} 
                      leftAvatar={<Avatar backgroundColor={this._getBackgroundColor(ld)}>A</Avatar>} 
                      secondaryText={ld.logMsg}>
                      {this._getDescription(ld)}
                      <div style={{ float: 'right' }}>
                        <FlatButton label="Replay Until Here" onClick={() => this._replayUntil(index) } />
                      </div>
                    </ListItem>
                  );
                })
              }
            </List>
          </CardText>
        </Card>
      </div>
    ); 
  }

  render() {
   return this.state.displayAllSessions ? this.renderSessionInfo() : this.renderSessionLogs(); 
  }

}

LogViewer.childContextTypes = {
  muiTheme: React.PropTypes.object
};
