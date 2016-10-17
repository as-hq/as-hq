const gitrev = require('git-rev-sync');
const path = require('path');
const webpack = require('webpack');

module.exports = {
  context: __dirname,
  entry: path.join(__dirname, 'src/js/index.jsx'),
  output: {
    path: path.join(__dirname, 'dist/js/'),
    filename: 'index.js',
    // publicPath: gulpPaths.SCRIPT_SOURCEMAP_PREFIX,
  },
  plugins: [
    // Note that order (AFAIK) is important here.
    // First deduplicate similar files.
    new webpack.optimize.DedupePlugin(),
    // Then give lower module / chunk ids to commonly used chunks
    new webpack.optimize.OccurenceOrderPlugin(),
    // `process.env.NODE_ENV: production` tells React it's running in
    // production mode... Don't console.{warn,error}. Note that the
    // JSON.stringify is important, so the resulting code goes from
    //
    // `if (process.env.NODE_ENV === "production")`
    //
    // to
    //
    // `if ("production" === "production")`
    //
    // rather than the error
    //
    // `if (production === "production")`.
    new webpack.DefinePlugin({
      __DEV__: false,
      'process.env': {
        NODE_ENV: JSON.stringify('production'),
      },
    }),
    // Finally, optimize the whole thing
    new webpack.optimize.UglifyJsPlugin(),
  ],
  resolve: {
    extensions: ['', '.js', '.jsx'],
  },
  module: {
    loaders: [
      {
        test: /\.jsx?$/,
        loaders: ['babel'],
        exclude: /node_modules/,
      },
      { // Turn off AMD module loading on eventemitter2
        test: /eventemitter2/,
        loader: 'imports?define=>false',
      }
    ]
  },
};
