var context = require.context('./int-tests', true, /keepup-test\.js$/);
context.keys().forEach(context);
console.log(context.keys());
