#' Round half away from zero
#'
#' Rounding where half is rounded away from zero
#' Wrapper to the round function in C
#'
#' @param x value to be rounded
#'
#' @return rounded value
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
#' @examples
#'
#' roundC(5.5)
#' roundC(-5.5)
#'
#' @export
roundC = function(x){
  if(is.matrix(x)){
    apply(x, c(1,2), function(y){
      roundC_wrapper(y)
    })
  } else{
    roundC_wrapper(x)
  }
}



#' Round half up
#'
#' Rounding where half is rounded up
#'
#' @param x value to be rounded
#'
#' @return rounded value
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
#' @examples
#'
#' rnd(5.5)
#'
#' @export
rnd = function(x){
  if(is.matrix(x)){
    apply(x, c(1,2), function(y){
      floor(y + 0.5)
    })
  } else{
    floor(x + 0.5)
  }
}
