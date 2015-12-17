/* @flow */

export default {
  isMac(): boolean {
    return window.navigator.platform.toUpperCase().indexOf('MAC') >= 0;
  },

  metaKeyName(): string {
    return this.isMac() ? "Cmd" : "Ctrl"; 
  }
};
