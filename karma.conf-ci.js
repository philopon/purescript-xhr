// Karma configuration
// Generated on Thu Dec 25 2014 22:32:54 GMT+0900 (JST)

module.exports = function(config) {

  var customLaunchers = {
    'SL_Chrome': {
      base: 'SauceLabs',
      browserName: 'chrome'
    },
    'SL_firefox': {
      base: 'SauceLabs',
      browserName: 'firefox'
    },
    'SL_IE11': {
      base: 'SauceLabs',
      browserName: 'internet explorer',
      version: '11'
    },
    'SL_IE10': {
      base: 'SauceLabs',
      browserName: 'internet explorer',
      version: '10'
    },
    'SL_IE9': {
      base: 'SauceLabs',
      browserName: 'internet explorer',
      version: '9'
    },
    'SL_Opera': {
      base: 'SauceLabs',
      browserName: 'opera'
    },
    'SL_Safari': {
      base: 'SauceLabs',
      browserName: 'safari',
      version: '8'
    },
    'SL_iOS8': {
      base: 'SauceLabs',
      browserName: 'iphone',
      version: '8.1'
    },
    'SL_iOS7': {
      base: 'SauceLabs',
      browserName: 'iphone',
      version: '7.1'
    },
    'SL_iOS6': {
      base: 'SauceLabs',
      browserName: 'iphone',
      version: '6.1'
    },
    'SL_Android4.4': {
      base: 'SauceLabs',
      browserName: 'android',
      version: '4.4'
    },
    'SL_Android4.2': {
      base: 'SauceLabs',
      browserName: 'android',
      version: '4.2'
    },
    'SL_Android4.0': {
      base: 'SauceLabs',
      browserName: 'android',
      version: '4.0'
    }
  };

  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',


    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['mocha'],


    // list of files / patterns to load in the browser
    files: [
      'tmp/test.js'
    ],

    proxies: {
      '/api': 'http://mockker.herokuapp.com'
    },


    // list of files to exclude
    exclude: [
    ],


    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
    },


    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['dots', 'saucelabs'],


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,

    sauceLabs: {
      testName: 'purescript-xhr'
    },

    captureTimeout: 1800000,
    customLaunchers: customLaunchers,

    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: Object.keys(customLaunchers),


    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
    singleRun: true
  });
};
