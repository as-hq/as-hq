/* @flow */

const Browser = {
  isMac(): boolean {
    return window.navigator.platform.toUpperCase().indexOf('MAC') >= 0;
  },

  metaKeyName(): string {
    return Browser.isMac() ? "⌘" : "Ctrl"; 
  }
};

export default Browser;