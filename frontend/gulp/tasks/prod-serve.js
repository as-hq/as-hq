var gulp = require('gulp');
var webserver = require('gulp-webserver');
var config = require('../config').prodServe;

gulp.task('prod-serve', ['prod-build'], () => {
  gulp.src(config.src)
    .pipe(webserver({
      port: config.port
    }));
});
