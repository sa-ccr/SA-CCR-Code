#' Calculates the Replacement Cost(RC) and the sum of the MtMs for all the trades
#' @title Calculates the RC
#' @param trades The full list of the Trade Objects
#' @return The replacement Cost and the sum of the MtMs
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm

CalcRC <- function(trades)  {
  
  V_C <- sum(sapply(trades, function(x) x$MtM))
  RC <- max(V_C,0)
return(list("V_C"=V_C,"RC"=RC))
}