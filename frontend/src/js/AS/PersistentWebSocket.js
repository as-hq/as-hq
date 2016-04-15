
/* @flow */

import type {
  Callback,
  IntervalId
} from '../types/Base';

// Behavior:
// - checks alive every 100ms
// - queues messages if connection loss detected
// - auto-reconnects

// How to use this class:
// const w = new pws();
// w.whenReady(() => doSomething());
// const url = getMyUrl();
// w.begin(url);             // doSomething will be executed when connection established
// w.send({'a': 1, 'b': 2}); // message will be queued + automatically sent

let ws = WebSocket;

import {logDebug} from './Logger';
import Constants from '../Constants.js'

const TRY_FLUSH_QUEUE_INTERVAL = 50; // try sending queued messages every n ms
const CHECK_ALIVE_INTERVAL = 100;    // perform the alive check every n ms
const HEARTBEAT_TIMEOUT = 4000;      // must receive heartbeat in n ms
const CONNECTION_TIMEOUT = 10000;    // must establish connection in n ms

class PersistentWebSocket {
  _url: string;
  _callbackQueue: Array<Callback<WebSocket>>;
  _client: WebSocket;
  _heartbeat: WebSocket;

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
  _ekg: IntervalId;
  _messagePump: IntervalId;
  _connectionWatch: IntervalId;
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


/******************************* Public setters ******************************/

  set onmessage(fn: Callback<MessageEvent>) {
    this._client.onmessage = (evt) => {
      this._onMessageInternal(evt);
      fn(evt);
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
    this._client = this._newClient(url);
    this._heartbeat = this._newHeartbeat(url);

    this._ready = true;
    this._readyCallbacks.forEach(cb => cb());

    // start main loops
    // (1) heartbeat verifier
    this._ekg = setInterval(() => {
      this._checkHeartbeat();
    }, CHECK_ALIVE_INTERVAL)

    // (2) message pump
    this._messagePump = setInterval(() => {
      this._tryFlushingQueue();
    }, TRY_FLUSH_QUEUE_INTERVAL);
  }

  whenReady(cb: Callback) {
    if (this._ready) {
      cb();
    } else {
      this._readyCallbacks.push(cb);
    }
  }

  // If 'prioritized', push message to front of the queue.
  //
  // #needsrefactor "prioritized" sending is a leaky abstraction
  // and should be replaced with a "handshake" setter. (I.e. you call
  // `pws.handshake = myLoginMessage` once, and every reconnection
  // attempt should send this "handshake" message before any other in the queue).
  send(msg: any, prioritized?: boolean) {
    const cb = (client) => {
      try {
        if (typeof msg === 'string') {
          client.send(msg);
        } else {
          client.send(JSON.stringify(msg));
        }
      } catch(err) {
        console.error(err);
      }
    };
    if (prioritized === true) {
      this._callbackQueue.unshift(cb);
    } else {
      this._callbackQueue.push(cb);
    }
    console.log('send queue: ', this._callbackQueue);
  }

  close() {
    console.log('Closed WS command from frontend');
    this._client.close();
    clearInterval(this._ekg);
    clearInterval(this._messagePump);
  }

  /******************************* Internal logic **************************/

  _canFlushQueue(): boolean {
    const {readyState, OPEN} = this._client;
    return
        (! this._isDisconnected)
        && (readyState === OPEN)
        && (this._timeoutCounter < HEARTBEAT_TIMEOUT);
  }

  _isHeartBeating(): boolean {
    const {readyState, CONNECTING, OPEN} = this._heartbeat;
    return (readyState === CONNECTING && ! this._connectionTimedOut())
        || (readyState === OPEN && ! this._heartbeatTimedOut());
  }

  _connectionTimedOut(): boolean {
    return this._timeoutCounter * CHECK_ALIVE_INTERVAL > CONNECTION_TIMEOUT;
  }

  _heartbeatTimedOut(): boolean {
    return this._timeoutCounter * CHECK_ALIVE_INTERVAL > HEARTBEAT_TIMEOUT;
  }

  _checkHeartbeat() {
    if (this._isHeartBeating()) {
      this._timeoutCounter++;
    } else {
      this._onConnectionLoss('heartbeat not found in time');
    }
  }

  _resetCounter() {
    this._timeoutCounter = 0;
  }

  _tryFlushingQueue() {
    while (this._callbackQueue.length > 0 && this._canFlushQueue()) {
      const cb = this._callbackQueue.shift();
      console.log('flushing callback in queue: ', q);
      cb(this._client);
    }
  }

  _onConnectionLoss(reason?: string) {
    console.error(`DETECTED CONNECTION LOSS (reason: ${reason})`);
    if (! this._isAttemptingReconnect) {
      // block other reconnection attempts until this one has finished
      this._isAttemptingReconnect = true;

      try {
        // call ondisconnect only the first time we detect a connection loss
        if (! this._isDisconnected) {
          this._ondisconnect();
        }
        // Keep track of our state as being currently disconnected or not.
        this._isDisconnected = true;
        this._closeConnections();
        this._attemptReconnect();
      } finally {
        this._isAttemptingReconnect = false;
      }
    }
  }

  _closeConnections() {
    this._client.close();
    this._heartbeat.close();
  }

  _attemptReconnect() {
    console.error("ATTEMPTING RECONNECT");

    this._heartbeat = this._newHeartbeat(this._url);
    this._client = this._revive(this._client, this._url);

    this._resetCounter();
  }

  _withNakedWS(fn: Callback<WebSocket>) { // FOR TESTING PURPOSES
    fn(this._client);
  }

  /******************************* constructors **************************/

  _newClient(url: string): WebSocket {
    const client = this._newConnection(url);
    // These setters are optional, but there is some internal logic
    // that hook into them anyway. Thus, we set that internal logic
    // as the default callbacks. They're not part of the PWS constructor
    // so that PWS can mirror the API of WS.
    client.onopen = this._onOpenInternal;
    client.onmessage = this._onMessageInternal;
    return client;
  }

  _newHeartbeat(url: string): WebSocket {
    const client = this._newConnection(url);

    client.onopen = () => {
      this._resetCounter();
      const msg = {
        tag: 'StartHeartbeat',
        contents: []
      };
      client.send(JSON.stringify(msg));
    }
    client.onmessage = () => this._resetCounter();
    return client;
  }

  _revive(oldWs: WebSocket, url: string): WebSocket {
    const {onmessage, onopen} = oldWs;
    const newWs = this._newConnection(url);
    newWs.onmessage = onmessage;
    newWs.onopen = onopen;
    return newWs;
  }

  _newConnection(url: string): WebSocket {
    try {
      return new ws(url);
    } catch(err) {
      return this._newConnection(url);
    }
  }

}

export default PersistentWebSocket;
