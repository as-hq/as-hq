module.exports = {
  'parser': 'babel-eslint',
  'extends': 'airbnb',

  'ecmaFeatures': {
    'experimentalObjectRestSpread': true,
  },

  'globals': {
    // TODO(joel)
    // '__DEV__': false,
    // '__COMMIT__': false,
  },

  'rules': {
    // places we differ from airbnb

    // always require curly braces
    'curly': 2,

    // no spaces inside object literals
    'object-curly-spacing': [2, 'never'],

    // no this references before super in class constructors
    'no-this-before-super': 2,

    // require spaces around => in arrow functions
    'arrow-spacing': 2,

    // ensure inheriting classes call super in constructor
    'constructor-super': 2,

    // prefer spread over Function.apply
    'prefer-spread': 2,

    // require yield in generator functions
    'require-yield': 2,

    // multiple components are allowed in a file, but only one should be exported
    'react/no-multi-comp': 0,

    // allow capital factory functions (immutable types, ES6 Symbol())
    'new-cap': [2, {'newIsCap': true, 'capIsNew': false}],

    // when unused args are present, they must start with double underscore
    'no-unused-vars': [2, {'args': 'all', 'argsIgnorePattern': '^__'}],
  },
};
