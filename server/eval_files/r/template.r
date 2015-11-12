# ::TODO:: why on earth are we referencing some file in a completely different directory? 
# this should change. 

suppressMessages(library("rjson"))
suppressMessages(library("jpeg"))
suppressMessages(library("party"))

graph <- function(x){
    graphics.off()
    fpath=paste0("/home/riteshr/asl-js/frontend/client/app/images/","graph",".png",sep="")
    if (file.exists(fpath)){
    }
    else {
      file.create(fpath)
    }
    png(filename=fpath)
    x
    graphics.off()
    return(list(imagePath=paste0("images/","graph",".png",sep="")))
}

isError = FALSE
result = tryCatch({
#CODE#
}, warning = function(w) {
	# nothing here
}, error = function(e) {
    isError <<- TRUE
	err = paste0("'error': \'Error: ", gsub("'",'"',e$message), "\'")
	errType = "'errType': \'try-error\'"
	position = "'position': -1" # TODO figure out line number of stacktrace in r
	file = paste0("'file': ", "\'", "run.r", "\'") #TODO get blamed file
	paste0("{", err, ", ", errType, ", ", position, ", ", file, "}")
}, finally = function() {
	# nothing here
})

# traceback()

if (isError) cat(result) else cat(toJSON(result))
