#' Calculates the Exposure at Default
#' @title Calculates the EAD
#' @param RC        the replacement cost
#' @param PFE       the projected future exposure
#' @return The Exposure-at-Default
#' @export
#' @examples
#' #returns 1.4*(60+500) = 784
#' EAD <- CalcEAD(60,500)
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj
CalcEAD <- function(RC, PFE)  {
  
  EAD <- 1.4*(RC+PFE)
  
  return(EAD)
}
