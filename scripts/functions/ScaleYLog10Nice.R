


scientific_10 <- function(x) {
  xout <- gsub("1e", "10^{", format(x),fixed=TRUE)
  xout <- gsub("{-0", "{-", xout,fixed=TRUE)
  xout <- gsub("{+", "{", xout,fixed=TRUE)
  xout <- gsub("{0", "{", xout,fixed=TRUE)
  xout <- paste(xout,"}",sep="")
  return(parse(text=xout))
}

scale_x_log10nice <- function(name=NULL,omag=seq(-10,20),...) {
  breaks10 <- 10^omag
  scale_x_log10(name,breaks=breaks10,labels=scientific_10(breaks10),...)
}

scale_y_log10nice <- function(name=NULL,omag=seq(-10,20),...) {
  breaks10 <- 10^omag
  scale_y_log10(name,breaks=breaks10,labels=scientific_10(breaks10),...)
}

scale_loglog <- function(...) {
  list(scale_x_log10nice(...),scale_y_log10nice(...))
}

# library(ggplot2)
# qplot(x=exp(5*rnorm(100)),geom="density",kernel="rectangular") + 
#   scale_x_log10nice()
