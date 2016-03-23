var dest = './dist';
var src = './src';
var gutil = require('gulp-util');

module.exports = {
  sass: {
    src: src + '/styles/**/*.{sass,scss,css}',
    dest: dest + '/styles',
    settings: {
      indentedSyntax: false, // Enable .sass syntax?
      imagePath: '/images' // Used by the image-url helper
    }
  },
  html: {
    src: 'src/*.html',
    dest: dest
  },
  watch: {
    src: 'src/**/*.*',
    tasks: ['build']
  },
  test: {
    karmaConfig: 'karma-test.conf.js',
    getConfig: function (fname) {
      var ret = {
        files:
          [].concat(
            [ 'node_modules/babel-core/browser-polyfill.js' ],
            [{
              pattern: fname,
              watched: false
            }]),
        preprocessors: {}
      };
      ret.preprocessors[fname] = ['webpack', 'sourcemap'];

      return ret;
    },
    evalFile: 'test-context-eval.js',
    excelFile: 'test-context-excel.js',
    formulaFile: 'test-context-formula.js',
    allFile: 'test-context-all.js'
  },
  stress: {
    karmaConfig: 'karma-stress.conf.js',
    getConfig: function (fname) {
      var ret = {
        files:
          [].concat(
            [ 'node_modules/babel-core/browser-polyfill.js' ],
            [{
              pattern: fname,
              watched: false
            }]),
        preprocessors: {}
      };
      ret.preprocessors[fname] = ['webpack'];

      return ret;
    },
    execFile: 'test-stress-all.js',
    numClients: 3,
  },
  prodServe: {
    src: dest,
    port: 8080
  }
};
