var gulp = require('gulp');
var config = require('../config').test;
var Server = require('karma').Server;

gulp.task('test-excel', function (done) {
  new Server(Object.assign({
    configFile: __dirname + '/../../' + config.karmaConfig
  }, config.getConfig(config.excelFile)), function () { done(); }).start();
});
