module.exports = function (config) {
  config.set({
    browsers: ['Chrome'],

    reporters: ['mocha'],

    files: [
      'node_modules/babel-core/browser-polyfill.js',
      { pattern: 'test-context.js', watched: false }
    ],

    frameworks: ['jasmine'],

    preprocessors: {
      'test-context.js': ['webpack', 'sourcemap']
    },

    singleRun: true,

    webpack: {
      devtool: 'inline-source-map',
      module: {
        loaders: [
          {
            test: /\.js/,
            exclude: /node_modules/,
            loader: 'babel-loader'
          }
        ],
        watch: true
      }
    },

    webpackServer: {
      noInfo: true
    },

    client: {
      captureConsole: false
    }
  });
};
