var context = require.context('./int-tests', true, /stress-test\.js$/);
context.keys().forEach(context);
console.log(context.keys());