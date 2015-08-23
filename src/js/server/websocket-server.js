var WebSocketServer = require("ws").Server;
var http = require("http");
var express = require("express");
var app = express();
var port = process.env.PORT || 5000;

app.use(express.static(__dirname + "/"));

var server = http.createServer(app);
server.listen(port);

console.log("http server listening on %d", port);

var wss = new WebSocketServer({server: server});
console.log("websocket server created");

var toServerMessageFormat = function (action, payloadTag, payload) {
    return ({
      "messageUserId": "testUserId",
      "action": action,
      "payload": {
        "tag": payloadTag,
        "contents": payload
      },
      "result": "Success"  //failure by default until server sets success
    });
};

var fakeCell = function(contents) {
   return {
      "cellLocation": {
        "tag": "Index",
        "index": contents.cellLocation.index
      },
      "cellExpression": {
        "expression" : contents.cellExpression.expression,
        "language" : contents.cellExpression.language
      },
      "cellValue":{
        "tag": "ValueS",
        "contents": JSON.stringify(contents.cellLocation.index)
      },
      "cellTags": []
  };
};

var getOrientedCorners = function(rng) {
  var tl = [Math.min(rng[0][0],rng[1][0]), Math.min(rng[0][1],rng[1][1])],
      br = [Math.max(rng[0][0],rng[1][0]), Math.max(rng[0][1],rng[1][1])];
  return [tl, br];
};

var interpolateLocations = function(rng) {
  if (rng[0].length) {
    var corners = getOrientedCorners(rng),
        tl = corners[0],
        br = corners[1],
        interpolated = [],
        idx=0;

    for (var r=tl[0]; r<br[0]+1; r++){
      for (var c=tl[1]; c<br[1]+1; c++){
        interpolated[idx] = [r,c]
        idx++;
      }
    }
    return interpolated;
  } else return rng;
};

var fakeCellFromIndex = function(idx) {
  return fakeCell({
      cellExpression: {
        expression: "TEST",
        language: "Python"
      },
      cellLocation: {
        tag: "Index",
        sheet: {sheetId: "testSheetId", sheetName: "Demo"},
        index: idx
      }
    });
}

var fakeCellFromASLoc = function(loc) {
  if (loc.tag === "Index")
    return fakeCell({
      cellExpression: {
        expression: "TEST",
        language: "Python"
      },
      cellLocation: loc
    });
  else {
    // console.log("generating fake cells for range: " + JSON.stringify(loc.range));
    var locs = interpolateLocations(loc.range);
    return locs.map(fakeCellFromIndex);
  }
};

wss.on("connection", function(ws) {
  console.log("websocket connection open");
  ws.on("message", function(message){
    console.log("message received by server: " + message); // the message will be an ASCell
    parsed = JSON.parse(message);

    var msg = null;
    // do some propagation in backend with message
    // for now, send a fake cell
    if (parsed.action === "Get") {
      if (parsed.payload.tag === "PayloadLL"){
        var cells = parsed.payload.contents.map(fakeCellFromASLoc);
        msg = toServerMessageFormat("NoAction", "PayloadCL", cells);
      } else if (parsed.payload.tag === "PayloadL"){
        var result = fakeCellFromASLoc(parsed.payload.contents);
        if (result.length)
          msg = toServerMessageFormat("NoAction", "PayloadCL", result);
        else
          msg = toServerMessageFormat("NoAction", "PayloadC", result);
      }
    } else if (parsed.action === "Evaluate") {
      var cell = fakeCell(parsed.payload.contents);
      msg = toServerMessageFormat("NoAction", "PayloadC", cell);
    }

    ws.send(JSON.stringify(msg));
    console.log("just sent data from server " );

  });

});

