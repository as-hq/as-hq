var gulp = require('gulp');
var config = require('../config').stress;
var Server = require('karma').Server;

gulp.task('stress-test', function (done) {

  // start n test servers
  for (let i = 0; i < config.numClients; i++) {
    new Server(Object.assign({
      configFile: __dirname + '/../../' + config.karmaConfig
    }, config.getConfig(config.execFile)), done).start();
  }
});
