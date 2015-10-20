var gulp = require('gulp');
var eslint = require('gulp-eslint');
var config = require('../config').watch;

gulp.task('lint', function () {
  return gulp.src(config.src)
    .pipe(eslint())
    .pipe(eslint.format());
});
