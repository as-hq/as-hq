/* @flow */

import type {
  Callback,
  IntervalId
} from '../types/Base';

// Behavior:
// - checks alive every 100ms
// - queues messages if readyState is 0
// - if the connection has spent too long at readyState 0, attempt to reconnect

let ws = WebSocket;

import {logDebug} from './Logger';
import Constants from '../Constants.js'

const QUEUE_FLUSH_INTERVAL = 50; // every n ms, try sending queued messages
const CHECK_ALIVE_INTERVAL = 100; // perform the alive check every n ms
const HEARTBEAT_TIMEOUT = 20; // if we don't receive a heartbeat at least once in this many intervals, timeout the connection

class PersistentWebSocket {
  _url: string;
  _callbackQueue: Array<Callback<WebSocket>>;
  _client: WebSocket;
  _onreconnect: () => void;
  _beforereconnect: () => void;
  _sendAck: Callback<WebSocket>;
  _intervalCounter: number;
  _heartbeat: IntervalId;
  _messagePump: IntervalId;
  _isDisconnected: boolean;

  constructor(url: string) {
    this._url = url;
    this._callbackQueue = [];
    this._client = new ws(url);
    this._intervalCounter = 0;
    this._isDisconnected = false;

    this._onreconnect = () => {
      this._isDisconnected = false;
      logDebug('Reconnected WS automatically after failure');
    };

    this._heartbeat = setInterval(() => {
      this._checkAliveAndPossiblyReconnect();
    }, CHECK_ALIVE_INTERVAL)

    this._messagePump = setInterval(() => {
      this._tryFlushingQueue();
    }, QUEUE_FLUSH_INTERVAL);
  }

  static install(w) {
    w.PersistentWebSocket = PersistentWebSocket;
  }

  set onmessage(fn: Callback<MessageEvent>) {
    this._client.onmessage = (evt) => {
      this._resetCounter();
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

  _alive(): boolean {
    return (this._client.readyState === 1)
        && (this._intervalCounter < HEARTBEAT_TIMEOUT);
  }

  _connecting(): boolean {
    return (this._client.readyState <= 1)
        && (this._intervalCounter < HEARTBEAT_TIMEOUT);
  }

  _checkAliveAndPossiblyReconnect() {
    if (this._connecting()) {
      this._intervalCounter++;
    } else {
      this.possiblyReconnect();
    }
  }

  _resetCounter() {
    this._intervalCounter = 0;
  }

  _tryFlushingQueue() {
    if (this._alive()) {
      while (this._callbackQueue.length > 0 && this._alive()) {
        let cb = this._callbackQueue.shift();
        cb(this._client);
      }
    } else {
      logDebug('Closed WS state:', this._client.readyState);
    }
  }

  _withNakedWS(fn: Callback<WebSocket>) { // FOR TESTING PURPOSES
    fn(this._client);
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

  possiblyReconnect() {
    this._client.close();
    if (Constants.shouldReconnect) {
      let {onmessage, onopen} = this._client;
      logDebug('Reconnecting...');
      // only execute _beforereconnect the first time
      // we realize we're disconnected
      if (!this._isDisconnected) {
        this._beforereconnect();
        this._isDisconnected = true;
      }

      this._client = new ws(this._url);
      this._client.onmessage = onmessage;
      this._client.onopen = onopen;
      this._resetCounter();
    }
  }

  close() {
    logDebug('Closed WS command from frontend');
    this._client.close();
    clearInterval(this._heartbeat);
    clearInterval(this._messagePump);
  }
}

export default PersistentWebSocket;
