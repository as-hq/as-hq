import devMiddleware from 'webpack-dev-middleware';
import express from 'express';
import gulp from 'gulp';
import gutil from 'gulp-util';
import path from 'path';
import webpack from 'webpack';

import devWebpackConf from '../../webpack.dev';


const PORT = 8080;
const dist = path.join(__dirname, '../../dist');


gulp.task('webpack-watch', () => {
  const app = express();
  const compiler = webpack({
    ...devWebpackConf,
    // source maps which optimize for accuracy
    devtool: 'source-map',
  });

  app.use(devMiddleware(compiler, {
    // TODO(joel) what does this do?
    noInfo: true,
    publicPath: devWebpackConf.output.publicPath,
  }));

  app.use(express.static(dist));

  app.get('/', (req, res) => {
    res.sendFile(path.join(dist, 'index.html'));
  });

  app.listen(PORT, 'localhost', err => {
    if (err) {
      throw new gutil.PluginError('webpack-dev-server', err);
    }

    gutil.log('[webpack-dev-server] on ' + PORT);
  });
});
