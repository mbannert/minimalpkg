#'@export
selectNumeric <- function(dframe){
  dframe[,sapply(dframe,is.numeric)]
}
