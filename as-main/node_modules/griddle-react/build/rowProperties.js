(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory(require("_"));
	else if(typeof define === 'function' && define.amd)
		define(["_"], factory);
	else if(typeof exports === 'object')
		exports["rowProperties"] = factory(require("_"));
	else
		root["rowProperties"] = factory(root["_"]);
})(this, function(__WEBPACK_EXTERNAL_MODULE_3__) {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId])
/******/ 			return installedModules[moduleId].exports;
/******/
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			exports: {},
/******/ 			id: moduleId,
/******/ 			loaded: false
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.loaded = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "/";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(0);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ function(module, exports, __webpack_require__) {

	module.exports = __webpack_require__(2);


/***/ },
/* 1 */,
/* 2 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";

	var _prototypeProperties = function (child, staticProps, instanceProps) { if (staticProps) Object.defineProperties(child, staticProps); if (instanceProps) Object.defineProperties(child.prototype, instanceProps); };

	var _classCallCheck = function (instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } };

	var _ = __webpack_require__(3);

	var RowProperties = (function () {
	  function RowProperties() {
	    var rowMetadata = arguments[0] === undefined ? {} : arguments[0];
	    _classCallCheck(this, RowProperties);

	    this.rowMetadata = rowMetadata;
	  }

	  _prototypeProperties(RowProperties, null, {
	    getRowKey: {
	      value: function getRowKey(row) {
	        var uniqueId;

	        if (this.hasRowMetadataKey()) {
	          uniqueId = row[this.rowMetadata.key];
	        } else {
	          uniqueId = _.uniqueId("grid_row");
	        }

	        //todo: add error handling

	        return uniqueId;
	      },
	      writable: true,
	      configurable: true
	    },
	    hasRowMetadataKey: {
	      value: function hasRowMetadataKey() {
	        return this.hasRowMetadata() && this.rowMetadata.key !== null && this.rowMetadata.key !== undefined;
	      },
	      writable: true,
	      configurable: true
	    },
	    hasRowMetadata: {
	      value: function hasRowMetadata() {
	        return this.rowMetadata !== null;
	      },
	      writable: true,
	      configurable: true
	    }
	  });

	  return RowProperties;
	})();

	module.exports = RowProperties;

/***/ },
/* 3 */
/***/ function(module, exports, __webpack_require__) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_3__;

/***/ }
/******/ ])
});
