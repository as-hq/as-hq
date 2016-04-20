import request from 'superagent';

let logLevel = 1;

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

export function logSlack(msg, channel) {
  const url = "https://hooks.slack.com/services/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";
  const json = {
    channel,
    username: "frontend-slack-bot",
    icon_emoji: "ghost",
    attachments: [{
      color: "#764FA5",
      text: msg,
      mrkdwn_in: [msg]
    }]
  };
  const req = request.post(url);
  req.send(JSON.stringify(json)).end((err, res) => {
    if (err || !res.ok) {
      console.error(err);
    } 
  });
}


export function logDebug() {
  if (logLevel <= 0) {
    Function.prototype.apply.apply(console.log, [console, arguments]);
    // disabling log.ly for now because it's polluting the console and not yet adding value. (Alex 1/4)
    // if (typeof(_LTracker) != "undefined") {
    //   _LTracker.push(arguments);
    // }
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
