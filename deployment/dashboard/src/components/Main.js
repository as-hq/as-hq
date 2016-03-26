require('normalize.css');
require('styles/App.css');

import React from 'react';
import Constants from '../Constants';
import JSONTree from 'react-json-tree';
import { FlatButton, Styles, List, ListItem, Divider } from 'material-ui';
import CircularProgress from 'material-ui/lib/circular-progress';
import request from  'superagent';

import _ from 'lodash';

window.request = request;

class Index extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      loadingReason: 'contacting master router...',
    };
    this._instances = {};
    request
      .post(Constants.master_router_url)
      .send({action: 'get_all_hosts'})
      .end((err, res) => {
        let hosts;
        if (res.status == 200) {
          hosts = JSON.parse(res.text);
        } else {
          console.error('Could not reach master router, at ' + Constants.master_router_host);
          hosts = [];
        }
        hosts.forEach(host => {
          this._instances[host] = [];
        });
        this.setState({loadingReason: null});
      });
  }

  componentDidMount() {
    setInterval(() => {
      if (this.state.loadingReason === null) {
        this._refreshInstances();
      }
    }, 1000);
  }

  render() {
    const {loadingReason} = this.state;
    return (
      <div className="index" style={styles.root}>
        <h1>AlphaSheets instance dashboard</h1>
        {!! loadingReason &&
          <div style={styles.loader}>
            <h3>{loadingReason}</h3>
            <CircularProgress size={2} color='white' />
          </div>
        }

        {_.map(this._instances, (hostData, host) =>
          <div>
            <div style={styles.host}>

              <h3>
                {host}
                <FlatButton
                  style={styles.createButton}
                  onClick={() => this._onCreate(host)}
                  color={Styles.Colors.grey50}>
                  spawn
                </FlatButton>
              </h3>

              {this._getHostContent(host, hostData)}

            </div>
            <br />
          </div>
        )}
      </div>
    );
  }

  _getContainerPane(host, container) {
    return (
      <div style={styles.container}>
        <FlatButton
          style={styles.destroyButton}
          onClick={() => this._onDestroy(host, container)}
          color={Styles.Colors.grey50}>
          destroy
        </FlatButton>
        <JSONTree data={container} />
      </div>
    );
  }

  _getHostContent(host, hostData) {
    if (hostData.constructor === Array) {
      return (
        <List>
          {hostData.map(container =>
            <div>
              <ListItem children={this._getContainerPane(host, container)} />
              <Divider />
            </div>
          )}
        </List>
      )
    } else {
      return (
        <h4>{typeof hostData == "string" ? hostData : JSON.stringify(hostData)}</h4>
      );
    }
  }

  _onCreate(host) {
    this.setState({loadingReason: 'fulfilling create request...'});
    request
      .post(this._getRouterUrl(host))
      .send({action: 'create'})
      .end((a, b) => {
        this.setState({loadingReason: null});
      });
  }

  _onDestroy(host, container) {
    this.setState({loadingReason: 'fulfilling destroy request...'});
    request
      .post(this._getRouterUrl(host))
      .send({
        action: 'destroy',
        name: container.name
      })
      .end((a, b) => {
        this.setState({loadingReason: null});
      });
  }

  _refreshInstances() {
    const self = this;
    _.forEach(this._instances, (containers, host) => {
      request
        .post(this._getRouterUrl(host))
        .send({action: 'get_all_status'})
        .end((err, res) => {
          if (!! res && res.status == 200) {
            const data = JSON.parse(res.text);
            if (data.length > 0) {
              self._instances[host] = data;
            } else {
              self._instances[host] = 'No active instances.'
            }
          } else {
            self._instances[host] = 'Could not reach host.';
          }
          self.forceUpdate();
        });
      });
  }

  _getRouterUrl(host) {
    return 'http://' + host + ':' + Constants.instance_router_port;
  }
}

const styles = {
  root: {
  },
  loader: {
    position: 'absolute',
    width: '100%',
    height: '100%',
    zIndex: 10,
    backgroundColor: 'rgba(0, 0, 0, 0.5)',
    textAlign: 'center'
  },
  createButton: {
    marginLeft: 20,
    position: 'inline',
    backgroundColor: Styles.Colors.blue900
  },
  destroyButton: {
    backgroundColor: Styles.Colors.red900
  },
  host: {
    padding: 2,
    backgroundColor: 'black',
    display: 'block',
    width: 'auto',
    height: 'auto',
  }
};

export default Index;
