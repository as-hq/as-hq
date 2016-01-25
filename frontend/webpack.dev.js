var path = require('path');
var webpack = require('webpack');

module.exports = {
  devtool: 'source-map',
  context: __dirname,
  entry: path.join(__dirname, 'src/js/index.jsx'),
  output: {
    path: path.join(__dirname, 'dist/js/'),
    filename: 'index.js',
    // publicPath: gulpPaths.SCRIPT_SOURCEMAP_PREFIX,
  },
  plugins: [
    new webpack.NoErrorsPlugin(),
    new webpack.DefinePlugin({
      __DEV__: true,
      'process.env': {
        NODE_ENV: JSON.stringify('development'),
      },
    }),
  ],
  resolve: {
    extensions: ['', '.js', '.jsx']
  },
  module: {
    loaders: [
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        loader: 'babel',
      },
      { // Turn off AMD module loading on eventemitter2
        test: /eventemitter2/,
        loader: 'imports?define=>false',
      }
    ]
  },
};
