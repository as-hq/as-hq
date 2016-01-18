var gulp = require('gulp');
var server = require('gulp-develop-server');
var config = require('../config').mockback;

gulp.task('mockback', function () {
  server.listen({ path: 'src/js/server/websocket-server.js' });
});
