// Behavior:
// - sends messages every 100ms
// - queues messages if readyState is 0
// - if the connection has spent too long at readyState 0, attempt to reconnect

import isNode from 'detect-node';
let ws = isNode ? require('ws') : WebSocket;

import {logDebug} from './Logger';

const TIMEOUT = 100; // number of timeout intervals to go
const INTERVAL = 50; // number of ms in an interval
const SEND_ACK_FREQ = 20;

class PersistentWebSocket {
  constructor(url) {
    this._url = url;
    this._callbackQueue = [];
    this._client = new ws(url);
    this._dcCount = 0;

    this._onreconnect = () => {
      logDebug('Reconnected WS automatically after failure');
    };

    this._messagePump = setInterval(() => {
      this._checkDC();
      this._sendQueue();
    }, INTERVAL);
  }

  static install(w) {
    w.PersistentWebSocket = PersistentWebSocket;
  }

  set onmessage(fn) {
    this._client.onmessage = (evt) => {
      this._dcCount = 0;
      fn(evt);
    };
  }

  set onopen(fn) {
    this._client.onopen = fn;
  }

  set beforereconnect(fn) {
    this._beforereconnect = fn;
  }

  set sendAck(fn) {
    this._sendAck = fn;
  }

  _checkDC() {
    this._dcCount++;
    if ((this._dcCount % SEND_ACK_FREQ === 0) && this._client.readyState === 1) {
      this._sendAck(this._client);
    }

    if (this._dcCount > TIMEOUT) {
      if (this._client.readyState === 1) {
        this._client.close();
      }

      let {onmessage, onopen} = this._client;

      this._beforereconnect();

      this._client = new ws(this._url);
      this._client.onmessage = onmessage;
      this._client.onopen = onopen;
      this._dcCount = 0;
    }
  }

  _sendQueue() {
    if (this._client.readyState === 1) {
      while (this._callbackQueue.length > 0 && this._client.readyState === 1) {
        let ele = this._callbackQueue.shift();
        ele(this._client);
      }
    } else if (this._client.readyState > 1) {
      logDebug('TIMING OUT DUE TO CLOSED WS');
      this._dcCount = TIMEOUT;
    }
  }

  _withNakedWS(fn) { // FOR TESTING PURPOSES
    fn(this._client);
  }

  readyState() {
    return this._client.readyState;
  }

  waitForConnection(cb) {
    this._callbackQueue.push(cb);
  }

  send(msg) {
    let self = this;
    this.waitForConnection(() => {
      self._client.send(msg);
    });
  }

  close() {
    this._client.close();
    clearInterval(this._messagePump);
  }
}

export default PersistentWebSocket;
