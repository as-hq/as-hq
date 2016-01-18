var context = require.context('./int-tests', true, /backend-test\.js$/);
context.keys().forEach(context);
console.log(context.keys());
