/* @flow */

export default {
  isMac(): boolean {
    return window.navigator.platform.toUpperCase().indexOf('MAC') >= 0;
  }
};
