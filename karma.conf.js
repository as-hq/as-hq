module.exports = function (config) {
  config.set({
    browsers: ['Chrome'],

    reporters: ['mocha'],

    mochaReporter: {
      colors: {
        success: 'green',
        info: 'yellow',
        warning: 'yellow',
        error: 'red'
      }
    },

    frameworks: ['jasmine'],

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
