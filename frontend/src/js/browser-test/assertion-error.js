function AssertionError(message) {
  this.name = 'AssertionError';
  this.message = message;
  this.stack = (new Error()).stack;
}

AssertionError.prototype = new Error;

export default AssertionError;
