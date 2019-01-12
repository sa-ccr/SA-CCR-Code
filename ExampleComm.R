#' Calculates the Exposure at Default for the Commodities example as given in the Basel III regulatory paper
#' @title Commodities Example
#' @param JSON (optional) if TRUE it returns a json string 
#' @return The exposure at default (expected value based on the Basel paper is 5406)
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm
 
ExampleComm =function(JSON = FALSE)
{
  requireNamespace("Trading")
tr1 = Trading::Commodity(external_id = "ext_1",Notional=10000,MtM= -50,Si=0,Ei=0.75,BuySell='Buy',SubClass='Energy',commodity_type='Oil/Gas')
tr2 = Trading::Commodity(external_id = "ext_2",Notional=20000,MtM= -30,Si=0,Ei=2,BuySell='Sell',SubClass='Energy',commodity_type='Oil/Gas')
tr3 = Trading::Commodity(external_id = "ext_3",Notional=10000,MtM= 100,Si=0,Ei=5,BuySell='Buy',SubClass='Metals',commodity_type='Silver')

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