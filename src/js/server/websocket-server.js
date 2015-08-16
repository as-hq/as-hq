var WebSocketServer = require("ws").Server
var http = require("http")
var express = require("express")
var app = express()
var port = process.env.PORT || 5000

app.use(express.static(__dirname + "/"))

var server = http.createServer(app)
server.listen(port)

console.log("http server listening on %d", port)

var wss = new WebSocketServer({server: server})
console.log("websocket server created")

var toServerMessageFormat = function (action, payloadTag, payload) {
    return ({
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
        "tag": "ValueD",
        "contents": 10
      }
  };
};

var fakeCellFromIndex = function(loc) {
  return fakeCell({
    cellExpression: {
      expression: "TEST",
      language: "Python"
    },
    cellLocation: {
      index: loc
    }
  });
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
        var cells = interpolateLocations(parsed.payload.contents).map(fakeCellFromIndex);
        msg = toServerMessageFormat("NoAction", "PayloadCL", cells);
      } else if (parsed.payload.tag === "PayloadL"){
        var cell = fakeCellFromIndex(parsed.payload.contents);
        msg = toServerMessageFormat("NoAction", "PayloadC", cell);
      }
    } else if (parsed.action === "Evaluate") {
      var cell = fakeCell(parsed.payload.contents);
      msg = toServerMessageFormat("NoAction", "PayloadC", cell);
    }

		ws.send(JSON.stringify(msg));
		console.log("just sent data from server " );

	});

});

