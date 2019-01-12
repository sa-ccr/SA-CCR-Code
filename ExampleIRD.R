#' Calculates the Exposure at Default for the IRD example as given in the Basel III regulatory paper
#' @title IRDs Example
#' @param JSON (optional) if TRUE it returns a json string 
#' @return The exposure at default (expected value based on the Basel paper is 569)
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm

ExampleIRD =function(JSON = FALSE)
{

  requireNamespace("Trading")
# creating the trade objects and storing them in a list
tr1 = Trading::IRDSwap(external_id = "ext_1",Notional=10000,MtM=30,Currency="USD",Si=0,Ei=10,BuySell='Buy')
tr2 = Trading::IRDSwap(external_id = "ext_2",Notional=10000,MtM=-20,Currency="USD",Si=0,Ei=4,BuySell='Sell')
tr3 = Trading::IRDSwaption(external_id = "ext_3",Notional=5000,MtM=50,Currency="EUR",Si=1,Ei=11,BuySell='Buy',OptionType='Put',UnderlyingPrice=0.06,StrikePrice=0.05)

trades= list(tr1,tr2,tr3)

csas = list()
colls = list()
# calculating the Exposure-at-Default
tree = runExampleCalcs(trades, csas, colls)

if(JSON==TRUE)
{
  requireNamespace("jsonlite")
  return(jsonlite::toJSON(as.list(tree[[1]])))
}
else
{
  return(tree[[1]])
}

}