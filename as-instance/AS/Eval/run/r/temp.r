
library("rjson")
library("jpeg")

graph <- function(name, x){
    graphics.off()
    fpath=paste0("/home/riteshr/asl-demo/frontend/client/app/images/",name,".png",sep="")
    if (file.exists(fpath)){
    }
    else {
      file.create(fpath)
    }
    png(filename=fpath)
    x
    graphics.off()
    return(list(imagePath=paste0("images/",name,".png",sep="")))
}

isError = FALSE
result = tryCatch({


<<<<<<< HEAD
list(c(0.0,1.0), c(0.0,1.0))
=======
20:40
>>>>>>> d7856a3c889dcf6f2e1efc46730c637375e37085
}, warning = function(w) {
	# nothing here
}, error = function(e) {
    isError <<- TRUE
	err = paste0("'error': \'Error: ", gsub("'",'"',e$message), "\'")
	err_type = "'err_type': \'try-error\'"
	position = "'position': -1" # TODO figure out line number of stacktrace in r
	file = paste0("'file': ", "\'", "temp.r", "\'") #TODO get blamed file
	paste0("{", err, ", ", err_type, ", ", position, ", ", file, "}")
}, finally = function() {
	# nothing here
})

# traceback()

if (isError) cat(result) else cat(toJSON(result))
