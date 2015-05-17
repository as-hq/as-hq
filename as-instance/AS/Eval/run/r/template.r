library("rjson")
library("jpeg")

as.graph <- function (name, f) {
    jpeg(file=name)
    f()
    dev.off()
}
