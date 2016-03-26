var gulp = require('gulp');
var config = require('../config').keepup;
var Server = require('karma').Server;

gulp.task('keepup', function (done) {
 new Server(Object.assign({
    configFile: __dirname + '/../../' + config.karmaConfig
  }, config.getConfig(config.keepupFile)), done).start();
});
