/* @flow */

const Browser = {
  isMac(): boolean {
    return window.navigator.platform.toUpperCase().indexOf('MAC') >= 0;
  },

  metaKeyName(): string {
    return Browser.isMac() ? "⌘" : "Ctrl"; 
  },

  isChrome(): boolean { 
    // http://stackoverflow.com/questions/4565112/javascript-how-to-find-out-if-the-user-browser-is-chrome
    return /Chrome/.test(navigator.userAgent) && /Google Inc/.test(navigator.vendor); 
  }
};

export default Browser;