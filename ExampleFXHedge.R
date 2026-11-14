#' Calculates the Exposure at Default for the FX product type
#' @title FX Example
#' @return The exposure at default
#' @param JSON (optional) if TRUE it returns a json string 
#' @param rwa_fx_cpty (optional) The risk weight of the original counterparty 
#' @param rwa_cds_cpty (optional) The risk weight of the hedging counterparty 
#' @param EAD_cds (optional) The EAD of the hedging CDS contract
#' @param hedging_approach (optional) The hedging approach, can be 'Current', 'TechnicalAmendment' or 'CappedProtection'
#' @param protection_percentage (optional) if the hedging_approach is 'CappedProtection'
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
#' @references Technical Amendment - Hedging of counterparty credit risk exposures
#' https://www.bis.org/bcbs/publ/d600.htm
#' @examples
#' tree_fx_hedge = ExampleFXHedge(rwa_fx_cpty = 0.2, rwa_cds_cpty = 0.2, EAD_cds = 14,
#'  hedging_approach = "CappedProtection", protection_percentage = 0.5)
ExampleFXHedge =function(JSON = FALSE, rwa_fx_cpty = NULL, rwa_cds_cpty = NULL, EAD_cds = NULL, hedging_approach = NULL, protection_percentage = NULL)
{
  requireNamespace("Trading")
  tr1 = Trading::FxForward(external_id = "ext_1",Notional=250,MtM=0,ccyPair="EUR/USD",Si=0,Ei=10,BuySell='Buy')

  trades= list(tr1)
  
  csas = list()
  colls = list()
  # calculating the Exposure-at-Default
  tree = runExampleCalcs(trades, csas, colls)
  
  if(!missing(rwa_fx_cpty)) tree[[1]]$fx_rwa = tree[[1]]$EAD*rwa_fx_cpty
  
  if(!missing(hedging_approach))
  {
    if(hedging_approach=='Current')
    { 
      unprotected_amount  = tree[[1]]$EAD-EAD_cds*rwa_fx_cpty
      protected_amount    = EAD_cds
    }else if(hedging_approach=='TechnicalAmendment')
    { 
      multiplier = 0.05 + 0.95 * exp(-EAD_cds/(1.9*tree[[1]]$addon))
      unprotected_amount = multiplier*tree[[1]]$EAD
      protected_amount   = (1-multiplier)*tree[[1]]$EAD
      
    }else if(hedging_approach=='CappedProtection')
    {
      multiplier = 0.05 + 0.95 * exp(-EAD_cds/protection_percentage/(1.9*tree[[1]]$addon))
      unprotected_amount = (1-protection_percentage)*tree[[1]]$EAD + protection_percentage*multiplier*tree[[1]]$EAD
      protected_amount   = tree[[1]]$EAD - unprotected_amount
    }else
    {stop("Wrong hedging_approach in the FX Hedging Example")}
    
    tree[[1]]$fx_rwa  = unprotected_amount*rwa_fx_cpty
    tree[[1]]$cds_rwa = protected_amount*rwa_cds_cpty
  }
  if(JSON==TRUE)
  {
    requireNamespace("jsonlite")
    return(jsonlite::toJSON(as.list(tree[[1]])))
  }
  else
  {  return(tree[[1]])}
  
}