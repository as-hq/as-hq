var context = require.context('./int-tests', true, /formula-test\.js$/);
context.keys().forEach(context);
console.log(context.keys());

