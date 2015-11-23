let logLevel = 0;

function arrayize(args) {
  let ret = new Array(args.length);
  for (let i = 0; i < args.length; i++) {
    ret[i] = args[i];
  }
  return ret;
}

function colorize(str) {
  return '%c' + str;
}

export function logDebug() {
  if (logLevel <= 0) {
    Function.prototype.apply.apply(console.log, [console, arguments]);
    if (typeof(_LTracker) != "undefined") {
      _LTracker.push(arguments);
    }
  }
}

export function logWarn(str) {
  if (logLevel <= 1) {
    console.log(colorize(str), 'color: #cd950c');
  }
}

export function logInfo(str) {
  if (logLevel <= 1) {
    console.log(colorize(str), 'color: #2f4f4f');
  }
}

export function logError(str) {
  if (logLevel <= 2) {
    console.log(colorize(str), 'color: #ff2400');
  }
}

export function logGreen(str) {
  if (logLevel <= 3) {
    console.log(colorize(str), 'color: #458b00');
  }
}

export function logRed(str) {
  if (logLevel <= 3) {
    console.log(colorize(str), 'color: #8b1a1a');
  }
}

export function setLogLevel(level) {
    /*
     * 0: normal
     * 1: test
     * higher: ???? (Michael forgot what they meant. -- Alex 11/23)
     */

  logLevel = level;
}

export function isTesting() {
  return logLevel > 0; 
}

export function setTestMode() {
  setLogLevel(1);
}

export function unsetTestMode() {
  setLogLevel(0);
}
