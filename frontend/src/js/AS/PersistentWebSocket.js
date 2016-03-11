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

const TRY_FLUSH_QUEUE_INTERVAL = 50; // every n ms, try sending queued messages
const CHECK_ALIVE_INTERVAL = 100; // perform the alive check every n ms
const HEARTBEAT_TIMEOUT = 20; // if we don't receive a heartbeat at least once in this many intervals, timeout the connection
const CHECK_CONNECTION_INTERVAL = 3000; // every 3s, check internet connection.

class PersistentWebSocket {
  _url: string;
  _callbackQueue: Array<Callback<WebSocket>>;
  _client: WebSocket;

// PersistentWebSocket is a wrapper around websocket, thus exposes the same API
// plus some (there are setters for callbacks like onmessage and onopen).
// There's some internal logic that relies on these callbacks, so that logic
// gets executed, and then the user-supplied callbacks execute after.

  _onreconnect: () => void;
  _ondisconnect: () => void;

  _onOpenInternal: (evt: Event) => void;
  _onMessageInternal: (evt: MessageEvent) => void;
  _ondisconnectInternal: () => void;
  _onreconnectInternal: () => void;

  _timeoutCounter: number;
  _heartbeat: IntervalId;
  _messagePump: IntervalId;
  _isDisconnected: boolean;
  _isAttemptingReconnect: boolean;

  _ready: boolean;
  _readyCallbacks: Array<Callback>;

  constructor() {
    this._callbackQueue = [];
    this._timeoutCounter = 0;
    this._isDisconnected = false;

    // used as a lock (MVar) to ensure that the various loops
    // don't interfere with each others' attempts to reestablish connection.
    this._isAttemptingReconnect = false;

    this._readyCallbacks = [];
    this._ready = false;

    // define internal callbacks that are always executed for proper operation
    this._onMessageInternal = (evt) => {
      this._resetCounter();
    }
    this._onOpenInternal = (evt) => {
      this._resetCounter();
      if (this._isDisconnected) {
        this._isDisconnected = false;
        this._onreconnect();
      }
    }
    this._ondisconnectInternal = () => {
      console.warn('WS disconnected.');
    }
    this._onreconnectInternal = () => {
      console.warn('WS reconnected after failure.');
    }


    this._ondisconnect = this._ondisconnectInternal;
    this._onreconnect = this._onreconnectInternal;
  }

  static install(w) {
    w.PersistentWebSocket = PersistentWebSocket;
  }


/******************************* Event callbacks ******************************/

  set onmessage(fn: Callback<MessageEvent>) {
    this._client.onmessage = (evt) => {
      this._onMessageInternal(evt);
      // swallow 'ACK's and 'PING's, because separation of concerns.
      if (evt.data === 'ACK' || evt.data === 'PING') {
        console.warn("Got heartbeat: " + evt.data);
      } else {
        fn(evt);
      }
    };
  }

  set onopen(fn: Callback<Event>) {
    this._client.onopen = (evt) => {
      this._onOpenInternal(evt);
      fn(evt);
    };
  }

  set onreconnect(fn: Callback) {
    this._onreconnect = () => {
      this._onreconnectInternal();
      fn();
    }
  }

  set ondisconnect(fn: Callback) {
    this._ondisconnect = () => {
      this._ondisconnectInternal();
      fn();
    }
  }


  /******************************* Public API *********************************/

  begin(url: string) {
    this._url = url;
    this._client = new ws(url);

    // These setters are optional, but there is some internal logic
    // that hook into them anyway. Thus, we set that internal logic
    // as the default callbacks. They're not part of the PWS constructor
    // so that PWS can mirror the API of WS.
    this._client.onopen = this._onOpenInternal;
    this._client.onmessage = this._onMessageInternal;

    this._ready = true;
    this._readyCallbacks.forEach(cb => cb());

    // start main loops

    // (1) hearbeat
    this._heartbeat = setInterval(() => {
      this._checkHeartbeat();
    }, CHECK_ALIVE_INTERVAL)

    // (2) message pump
    this._messagePump = setInterval(() => {
      this._tryFlushingQueue();
    }, TRY_FLUSH_QUEUE_INTERVAL);

    // (3) internet connection detector
    this._connectionWatch = setInterval(() => {
      this._checkConnection();
    }, CHECK_CONNECTION_INTERVAL);
  }

  whenReady(cb: Callback) {
    if (this._ready) {
      cb();
    } else {
      this._readyCallbacks.push(cb);
    }
  }

  waitForConnection(cb: Callback<WebSocket>) {
    this._callbackQueue.push(cb);
  }

  send(msg: string) {
    this.waitForConnection(() => this._client.send(msg));
  }

  close() {
    logDebug('Closed WS command from frontend');
    this._client.close();
    clearInterval(this._heartbeat);
    clearInterval(this._messagePump);
    clearInterval(this._connectionWatch);
  }

  /******************************* Private functions **************************/

  _alive(): boolean {
    return (this._client.readyState === 1)
        && (this._timeoutCounter < HEARTBEAT_TIMEOUT);
  }

  _connecting(): boolean {
    const {readyState} = this._client;
    // if the readyState is 1, we can only trust that the socket is properly bound
    // and backend is simply taking a long time to initialize (this is empirically
    // true when connecting remotely). Otherwise, there is the possibility that
    // the client will prematurely close the connection. If the readyState is 0
    // (in connecting phase), then we will timeout for the heartbeat, since the
    // very first thing backend does upon connection is start a heartbeat.
    return (readyState === 1)
        || (readyState === 0 && this._timeoutCounter < HEARTBEAT_TIMEOUT);
  }

  _checkHeartbeat() {
    if (this._connecting()) {
      this._timeoutCounter++;
    } else {
      this._onConnectionLoss();
    }
  }

  _checkConnection() {
    if (! hasInternetConnection() || this._isDisconnected) {
      this._onConnectionLoss();
    }
  }

  _resetCounter() {
    this._timeoutCounter = 0;
  }

  _tryFlushingQueue() {
    if (this._alive()) {
      while (this._callbackQueue.length > 0 && this._alive()) {
        let cb = this._callbackQueue.shift();
        cb(this._client);
      }
    }
  }

  _withNakedWS(fn: Callback<WebSocket>) { // FOR TESTING PURPOSES
    fn(this._client);
  }

  _onConnectionLoss() {
    if (! this._isAttemptingReconnect) {
      this._isAttemptingReconnect = true;

      // call ondisconnect only the first time we detect a connection loss
      if (! this._isDisconnected) {
        this._ondisconnect();
      }
      // Keep track of our state as being currently disconnected or not.
      this._isDisconnected = true;
      this._client.close();
      if (Constants.shouldReconnect) {
        this._attemptReconnect();
      }

      this._isAttemptingReconnect = false;
    }
  }

  _attemptReconnect() {
    console.log("attempting reconnect");
    let {onmessage, onopen} = this._client;
    this._client = new ws(this._url);
    this._client.onmessage = onmessage;
    this._client.onopen = onopen;
    this._resetCounter();
  }

}

function hasInternetConnection() {
  // Handle IE and more capable browsers
  const xhr = new ( window.ActiveXObject || XMLHttpRequest )( "Microsoft.XMLHTTP" );

  // Open new request as a HEAD with a random param to bust the cache
  xhr.open("HEAD", "http://enable-cors.org" +  "/?rand=" + Math.floor((1 + Math.random()) * 0x10000), false);

  // Issue request and handle response
  try {
    xhr.send();
    return ( xhr.status >= 200 && (xhr.status < 300 || xhr.status === 304) );
  } catch (error) {
    return false;
  }
}

export default PersistentWebSocket;
