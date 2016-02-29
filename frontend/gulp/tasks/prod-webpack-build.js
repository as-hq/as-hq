import gulp from 'gulp';
import gutil from 'gulp-util';
import env from 'gulp-env';
import webpack from 'webpack';

import prodWebpackConf from '../../webpack.prod';

gulp.task('set-prod-env', () => {
  env({
    vars: {
      NODE_ENV: 'production'
    }
  });
});

gulp.task('prod-webpack-build', ['set-prod-env'], callback => {
  webpack(prodWebpackConf, (err, stats) => {
    if (err) {
      throw new gutil.PluginError('webpack', err);
    }

    gutil.log('[webpack]', stats.toString());
    callback();
  });
});
