#' Calculates the exposure at Default
#' 
#' Takes as inputs the replacement cost and the projected future exposure
#' @param RC the replacement cost
#' @param PFE the projected future exposure
#' @return The exposure at default
#' @export
#' @examples
#' 
#' #returns 1.4*(60+500) = 784
#' EAD <- CalcEAD(60,500)
#' 
#' 
CalcEAD <- function(RC, PFE)  {
  
  EAD = 1.4*(RC+PFE)
  
  return(EAD)
}