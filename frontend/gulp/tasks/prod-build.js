var gulp = require('gulp');

gulp.task('prod-build', ['prod-webpack-build', 'styles', 'html']);
