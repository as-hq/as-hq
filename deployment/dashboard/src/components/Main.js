require('normalize.css');
require('styles/App.css');

import React from 'react';
import Constants from '../Constants';
import JSONTree from 'react-json-tree';
import { FlatButton, Styles, List, ListItem, Divider } from 'material-ui';

import _ from 'lodash';

class Index extends React.Component {

  constructor(props) {
    super(props);
    this._instances = {};
    Constants['monitored_hosts'].forEach(host => {
      this._instances[host] = [];
    });
  }

  componentDidMount() {
    setInterval(() => {
      this._refreshInstances();
      this.forceUpdate();
    }, 1000);
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
    const xhr = new XMLHttpRequest();
    xhr.open('POST', this._getRouterUrl(host), true);
    xhr.send(JSON.stringify({
      action: 'create'
    }));
  }

  _onDestroy(host, container) {
    const xhr = new XMLHttpRequest();
    xhr.open('POST', this._getRouterUrl(host), true);
    xhr.send(JSON.stringify({
      action: 'destroy',
      name: container.name
    }));
  }

  _refreshInstances() {
    Constants['monitored_hosts'].forEach(host => {
      const xhr = new XMLHttpRequest();
      const self = this;
      xhr.onload = () => {
        self._instances[host] =
          JSON.parse(xhr.getResponseHeader('statuses')) ||
          [{content: 'none'}];
      };
      xhr.open('POST', this._getRouterUrl(host), true);
      xhr.send(JSON.stringify({
        action: 'get_all_status'
      }));
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
