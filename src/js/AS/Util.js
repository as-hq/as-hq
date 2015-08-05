import Constants from '../Constants';

export default {

  getLanguageFromEngine(eng) {
    switch(eng) {
      case Constants.Engine.Python:
        return 'python';
    }
  },

  arrContains(arr, elem) {
    return arr.indexOf(elem) > -1;
  },

  showValue(val){
    switch (val.tag){
      case "ValueS":
        return val.contents;
      case "ValueD":
        return val.contents.toString();
      // etc
    }
  }
};
