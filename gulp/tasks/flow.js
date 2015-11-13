var gulp = require('gulp');
var babel = require('gulp-babel');
var flow = require('gulp-flowtype');
var notify = require('gulp-notify');
var sourcemaps = require('gulp-sourcemaps');
var sourcemapReporter = require('jshint-sourcemap-reporter');

var debug = require('gulp-debug');

var config = require('../config.js').flow;

gulp.task('flow:babel', (cb) => {
  gulp.src(config.src + '/**/*.{js,jsx}')
    .pipe(sourcemaps.init())
    .pipe(babel({ blacklist: ['flow'] }))
    .on('error', notify.onError((error) => error.message))
    .pipe(sourcemaps.write('.'))
    .pipe(gulp.dest(config.dest))
    .on('end', cb);
});

gulp.task('flow', ['flow:babel'], (cb) => {
  gulp.src(config.dest + '/**/*.js')
    .pipe(debug())
    .pipe(flow({
      declarations: './declarations',
      reporter: {
        reporter: function (errors) {
          return sourcemapReporter.reporter(
            errors, { sourceRoot: config.src + '/' }
          );
        }
      }
    }));
});

gulp.task('flow:watch', () => {
  gulp.watch(config.src + '**/*.js', ['flow']);
});
