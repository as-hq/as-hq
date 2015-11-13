/* @flow */

import type {
  Callback,
  IntervalId
} from '../types/Base';

// Behavior:
// - sends messages every 100ms
// - queues messages if readyState is 0
// - if the connection has spent too long at readyState 0, attempt to reconnect

let ws = WebSocket;

import {logDebug} from './Logger';

const TIMEOUT = 100; // number of timeout intervals to go
const INTERVAL = 50; // number of ms in an interval
const SEND_ACK_FREQ = 20;

class PersistentWebSocket {
  _url: string;
  _callbackQueue: Array<Callback<WebSocket>>;
  _client: WebSocket;
  _dcCount: number;
  _onreconnect: () => void;
  _beforereconnect: () => void;
  _sendAck: Callback<WebSocket>;
  _messagePump: IntervalId;

  constructor(url: string) {
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

  set onmessage(fn: Callback<MessageEvent>) {
    this._client.onmessage = (evt) => {
      this._dcCount = 0;
      fn(evt);
    };
  }

  set onopen(fn: Callback<Event>) {
    this._client.onopen = fn;
  }

  set beforereconnect(fn: Callback) {
    this._beforereconnect = fn;
  }

  set sendAck(fn: Callback<WebSocket>) {
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
      // logDebug('TIMING OUT DUE TO CLOSED WS');
      this._dcCount = TIMEOUT;
    }
  }

  _withNakedWS(fn: Callback<WebSocket>) { // FOR TESTING PURPOSES
    fn(this._client);
  }

  readyState(): number {
    return this._client.readyState;
  }

  waitForConnection(cb: Callback<WebSocket>) {
    this._callbackQueue.push(cb);
  }

  send(msg: string) {
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
