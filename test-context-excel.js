var context = require.context('./int-tests', true, /excel-test\.js$/);
context.keys().forEach(context);
console.log(context.keys());
