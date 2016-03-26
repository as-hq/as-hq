require('normalize.css');
require('styles/App.css');

import React from 'react';
import Constants from '../Constants';
import JSONTree from 'react-json-tree';
import { FlatButton, Styles, List, ListItem, Divider } from 'material-ui';
import request from  'superagent';

import _ from 'lodash';

class Index extends React.Component {

  constructor(props) {
    super(props);
    this._instances = {};
    let hosts;
    request
      .post(this._getRouterUrl(Constants.master_router_host))
      .send({action: 'get_all_hosts'})
      .end((err, res) => {
        if (res.status == 200) {
          hosts = res.body;
        } else {
          console.error('Could not reach master router, at ' + Constants.master_router_host);
          hosts = [];
        }
      });
    hosts.forEach(host => {
      this._instances[host] = [];
    });
  }

  componentDidMount() {
    setInterval(() => {
      this._refreshInstances();
    }, 500);
  }

  render() {
    return (
      <div className="index" style={styles.root}>
        <h1>AlphaSheets instance dashboard</h1>
        {_.map(this._instances, (containers, host) =>
          <div>
            <div style={styles.host}>

              <h3>
                {host}
                <FlatButton
                  style={styles.createButton}
                  onClick={() => this._onCreate(host)}
                  color={Styles.Colors.grey50}>
                  create
                </FlatButton>
              </h3>

              <List>
                {containers.map(container =>
                  <div>
                    <ListItem children={this._getContainerPane(host, container)} />
                    <Divider />
                  </div>
                )}
              </List>

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

  _onCreate(host) {
    request
      .post(this._getRouterUrl(host))
      .send(JSON.stringify({
        action: 'create'
      }));
  }

  _onDestroy(host, container) {
    request
      .post(this._getRouterUrl(host))
      .send(JSON.stringify({
        action: 'destroy',
        name: container.name
      }));
  }

  _refreshInstances() {
    const self = this;
    _.forEach(this._instances, (containers, host) => {
      request
        .post(this._getRouterUrl(host))
        .send({action: 'get_all_status'})
        .end((err, res) => {
          if (res.status == 200) {
            self._instances[host] = res.body;
            self.forceUpdate();
          } else {
            console.error('Could not reach host: ' + host);
          }
        });
      });
  }

  _getRouterUrl(host) {
    return 'http://' + host + ':' + Constants.router_port;
  }
}

const styles = {
  root: {
    backgroundColor: 'black'
  },
  createButton: {
    marginLeft: 20,
    position: 'inline',
    backgroundColor: Styles.Colors.green900
  },
  destroyButton: {
    backgroundColor: Styles.Colors.amber900
  },
  host: {
    display: 'block',
    width: 'auto',
    height: 'auto',
  }
};

export default Index;
