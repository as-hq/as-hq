
library("rjson")
library("jpeg")

as.graph <- function (name, f) {
    jpeg(file=name)
    f()
    dev.off()
}


cat(toJSON(1+1))