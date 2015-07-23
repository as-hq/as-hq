
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
form <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
dtree <- ctree(form, data=iris)

graph(plot(dtree,type = "simple"))
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
