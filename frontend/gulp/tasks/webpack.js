import gulp from 'gulp';
import gutil from 'gulp-util';
import webpack from 'webpack';

import prodWebpackConf from '../../webpack.prod';

gulp.task('webpack', callback => {
  webpack(prodWebpackConf, (err, stats) => {
    if (err) {
      throw new gutil.PluginError('webpack', err);
    }

    gutil.log('[webpack]', stats.toString());
    callback();
  });
});
