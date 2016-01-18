var context = require.context('./int-tests', true, /-test\.js$/);
context.keys().forEach(context);
console.log(context.keys());
