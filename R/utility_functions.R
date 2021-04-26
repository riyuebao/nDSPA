#' nDSPA Utility


#Convert factor/character/numeric to numeric
#' @export
to.numeric <- function(x) as.numeric(as.character(x))

#' @export
#Geomean calculated by log(array) -> mean(array) -> exp(val)
gmean <- function(x,method="log") {
  if (method == "log"){
    #Safer method does not produce overflows
    gm <- exp(mean(log(x)))

  }else if (method == "mult"){
    gm <- prod(x)^(1/length(x))
  }
  return(gm)
}
