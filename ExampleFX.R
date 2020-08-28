#' Calculates the Exposure at Default for the FX product type
#' @title FX Example
#' @return The exposure at default
#' @param JSON (optional) if TRUE it returns a json string 
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm

ExampleFX =function(JSON = FALSE)
{
  requireNamespace("Trading")
tr1 = Trading::FxForward(external_id = "ext_1",Notional=10000,MtM=30,ccyPair="EUR/USD",Si=0,Ei=10,BuySell='Buy')
tr2 = Trading::FxForward(external_id = "ext_2",Notional=20000,MtM=-20,ccyPair="EUR/USD",Si=0,Ei=4,BuySell='Sell')
tr3 = Trading::FxForward(external_id = "ext_3",Notional=5000,MtM=50,ccyPair="GBP/USD",Si=1,Ei=11,BuySell='Sell')

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
{  return(tree[[1]])}

}