'%ni%' <- function(x, y) {
  '%op%' <- Negate('%in%')
  return(x %op% y)
}


# syntactic sugar
# ******************************************************************************
#' inline operator for string concatenation based on paste0
#'
#' @param x string1
#' @param y string2
#'
#' @return paste0(x, y)
#' @export
"%+%" <- function (x,y) {
  paste0(x,y)
}