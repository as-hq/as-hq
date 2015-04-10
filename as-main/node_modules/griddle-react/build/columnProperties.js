(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory(require("_"));
	else if(typeof define === 'function' && define.amd)
		define(["_"], factory);
	else if(typeof exports === 'object')
		exports["columnProperties"] = factory(require("_"));
	else
		root["columnProperties"] = factory(root["_"]);
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

	module.exports = __webpack_require__(1);


/***/ },
/* 1 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";

	var _prototypeProperties = function (child, staticProps, instanceProps) { if (staticProps) Object.defineProperties(child, staticProps); if (instanceProps) Object.defineProperties(child.prototype, instanceProps); };

	var _classCallCheck = function (instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } };

	var _ = __webpack_require__(3);

	var ColumnProperties = (function () {
	  function ColumnProperties() {
	    var allColumns = arguments[0] === undefined ? [] : arguments[0];
	    var filteredColumns = arguments[1] === undefined ? [] : arguments[1];
	    var childrenColumnName = arguments[2] === undefined ? "children" : arguments[2];
	    var columnMetadata = arguments[3] === undefined ? [] : arguments[3];
	    var metadataColumns = arguments[4] === undefined ? [] : arguments[4];
	    _classCallCheck(this, ColumnProperties);

	    this.allColumns = allColumns;
	    this.filteredColumns = filteredColumns;
	    this.childrenColumnName = childrenColumnName;
	    this.columnMetadata = columnMetadata;
	    this.metadataColumns = metadataColumns;
	  }

	  _prototypeProperties(ColumnProperties, null, {
	    getMetadataColumns: {
	      value: function getMetadataColumns() {
	        var meta = _.map(_.where(this.columnMetadata, { visible: false }), function (item) {
	          return item.columnName;
	        });
	        if (meta.indexOf(this.childrenColumnName) < 0) {
	          meta.push(this.childrenColumnName);
	        }
	        return meta.concat(this.metadataColumns);
	      },
	      writable: true,
	      configurable: true
	    },
	    getVisibleColumnCount: {
	      value: function getVisibleColumnCount() {
	        return this.getColumns().length;
	      },
	      writable: true,
	      configurable: true
	    },
	    getColumnMetadataByName: {
	      value: function getColumnMetadataByName(name) {
	        return _.findWhere(this.columnMetadata, { columnName: name });
	      },
	      writable: true,
	      configurable: true
	    },
	    hasColumnMetadata: {
	      value: function hasColumnMetadata() {
	        return this.columnMetadata !== null && this.columnMetadata.length > 0;
	      },
	      writable: true,
	      configurable: true
	    },
	    isColumnSortable: {
	      value: function isColumnSortable(name) {
	        var meta = this.getColumnMetadataByName(name);

	        //allow sort if meta isn't there
	        if (typeof meta === "undefined" || meta === null) {
	          return true;
	        }return meta.hasOwnProperty("sortable") ? meta.sortable : true;
	      },
	      writable: true,
	      configurable: true
	    },
	    getColumns: {
	      value: function getColumns() {
	        var _this = this;
	        var ORDER_MAX = 100;
	        //if we didn't set default or filter
	        var filteredColumns = this.filteredColumns.length === 0 ? this.allColumns : this.filteredColumns;

	        filteredColumns = _.difference(filteredColumns, this.metadataColumns);

	        filteredColumns = _.sortBy(filteredColumns, function (item) {
	          var metaItem = _.findWhere(_this.columnMetadata, { columnName: item });

	          if (typeof metaItem === "undefined" || metaItem === null || isNaN(metaItem.order)) {
	            return ORDER_MAX;
	          }

	          return metaItem.order;
	        });

	        return filteredColumns;
	      },
	      writable: true,
	      configurable: true
	    }
	  });

	  return ColumnProperties;
	})();

	module.exports = ColumnProperties;

/***/ },
/* 2 */,
/* 3 */
/***/ function(module, exports, __webpack_require__) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_3__;

/***/ }
/******/ ])
});
