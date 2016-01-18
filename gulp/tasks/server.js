var gulp = require('gulp');
var connect = require('gulp-connect');
var config = require('../config').server;
var argv = require('yargs').argv;

gulp.task('server', function() {
	process.env.NODE_ENV = argv.production ? 'production' : 'development';
	var ports;
	if (argv.ports) {
		if (typeof argv.ports === 'number') ports = [JSON.stringify(argv.ports)];
		else ports = argv.ports.split(',');
	} else ports = [config.settings.defaultPort];
	ports.forEach(function(port) {
		config.settings.port = port;
		connect.server(config.settings);
	});
});
