#' Returns a tree structure depicting the add-on calculations on different hedging/netting sets
#' @title SA-CCR Calculator
#' @param trades_filename a .csv file containing the trades
#' @param csa_filename a .csv file containing CSAs
#' @param coll_filename a .csv file containing collaterals
#' @param JSON (optional) if TRUE it returns a json string 
#' @return The exposure at default (expected value based on the Basel paper is 569)
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm

SACCRCalculator = function(trades_filename, csa_filename, coll_filename, JSON = FALSE)
{
  requireNamespace("Trading")
  trades = Trading::ParseTrades(trades_filename)
  
  csas = list()
  if(!missing(csa_filename))
  {
  csa_raw = read.csv(csa_filename)
  
  
  for(i in 1:nrow(csa_raw))
  {
    csas[[i]] = Trading::CSA()
    csas[[i]]$PopulateViaCSV(csa_raw[i,])
  }
  }
  colls = list()
  if(!missing(coll_filename))
  {
  coll_raw = read.csv(coll_filename)
  
  for(i in 1:nrow(coll_raw))
  {
    colls[[i]] = Trading::Collateral()
    colls[[i]]$PopulateViaCSV(coll_raw[i,])
  }
  }
  trees = runExampleCalcs(trades, csas, colls)
  
  if(JSON==TRUE)
  {
    requireNamespace("jsonlite")
    return(jsonlite::toJSON(as.list(trees[[1]])))
  }
  else
  {
  return(trees)
  }
}
  