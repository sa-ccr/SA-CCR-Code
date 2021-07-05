#' Calculates the Projected Future Exposure (PFE) after applying the relevant multiplier.
#' The purpose of this multiplier is to lessen the risk stemming from the addons in case of excess collateral
#' @title Calculates the PFE
#' @param V_C the difference between the sum of the MtMs and the collateral
#' @param V the sum of MVs of the trades
#' @param Addon_Aggregate the aggregate amount of the Addon
#' @param simplified (optional) When TRUE, the multiplier will be set to 1  as per the simplified & OEM approach
#' @return The Projected Future Exposure (PFE)
#' @export
#' @author Tasos Grivas <info@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj

CalcPFE <- function(V_C,Addon_Aggregate, simplified,V=0)  {
  
  if(missing(V)) V=V_C
  
  V_C= min(V_C,V)
  if(simplified)
  {  multiplier = 1
  }else
  {
    if (V_C<0){
      multiplier = min( 1, 0.05 + 0.95 * exp(V_C/(1.9*Addon_Aggregate)))
    }else  
    { multiplier = 1}
  }
  
  PFE = multiplier * Addon_Aggregate
  
  return(PFE)
}