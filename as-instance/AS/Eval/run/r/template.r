library("rjson")
library("jpeg")

as.graph <- function (name, f) {
    jpeg(file=name)
    f()
    dev.off()
}

result = tryCatch({
#CMD#
}, warning = function(w) {
	# nothing here
}, error = function(e) {
	err = paste0("'error': \'Error: ", gsub("'",'"',e$message), "\'")
	err_type = "'err_type': \'try-error\'"
	position = "'position': -1" # TODO figure out line number of stacktrace in r
	file = paste0("'file': ", "\'", "temp.r", "\'") #TODO get blamed file
	paste0("{", err, ", ", err_type, ", ", position, ", ", file, "}")
}, finally = function() {
	# nothing here
})

# traceback()

cat(result)
