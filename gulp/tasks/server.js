var gulp = require('gulp');
var connect = require('gulp-connect');
var config = require('../config').server;
var argv = require('yargs').argv;

gulp.task('server', function() {
	var ports = argv.ports ? argv.ports.split(',') : [config.settings.defaultPort];
	ports.forEach(function(port) {
		config.settings.port = port;
		connect.server(config.settings);
	});
});
