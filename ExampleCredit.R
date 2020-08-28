#' Calculates the Exposure at Default for the Credit example as given in the Basel III regulatory paper
#' @title Credit Products Example
#' @param JSON (optional) if TRUE it returns a json string 
#' @return The exposure at default (expected value based on the Basel paper is 381)
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm

ExampleCredit =function(JSON = FALSE)
{
  requireNamespace("Trading")
tr1 = Trading::CDS(external_id = "ext_1",Notional=10000,MtM=20,Currency="USD",Si=0,Ei=3,BuySell='Buy',SubClass='AA',RefEntity='FirmA')
tr2 = Trading::CDS(external_id = "ext_2",Notional=10000,MtM=-40,Currency="EUR",Si=0,Ei=6,BuySell='Sell',SubClass='BBB',RefEntity='FirmB')
tr3 = Trading::CDX(external_id = "ext_3",Notional=10000,MtM=0,Currency="USD",Si=0,Ei=5,BuySell='Buy',SubClass='IG',RefEntity='CDX.IG')

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