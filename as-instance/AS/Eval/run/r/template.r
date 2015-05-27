library("rjson")
library("jpeg")

graph <- function(x){
    name="test"
    graphics.off()
    fpath=paste("/home/hal/code/alphasheets/frontend/client/app/images/",name,".png",sep="")
    if (file.exists(fpath)){
    }
    else {
      file.create(fpath)
    }
    png(filename=fpath)
    x
    graphics.off()
    return(list(imagePath=paste("images/",name,".png",sep="")))
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
