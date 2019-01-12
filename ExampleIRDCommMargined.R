#' Calculates the Exposure at Default for the margined IRDs + Commodity example as given in the Basel III regulatory paper
#' @title Margined IRDs+Commodity Example
#' @param JSON (optional) if TRUE it returns a json string 
#' @return The exposure at default (expected value  based on the Basel paper is 1879)
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Basel Committee: The standardised approach for measuring counterparty credit risk exposures
#' http://www.bis.org/publ/bcbs279.htm

ExampleIRDCommMargined =function(JSON = FALSE)
{
  requireNamespace("Trading")
  
tr1 = Trading::Commodity(external_id = "ext_1",Notional=10000,MtM= -50,Si=0,Ei=0.75,BuySell='Buy',SubClass='Energy',commodity_type='Oil/Gas',Currency="USD", Counterparty = "cpty1")
tr2 = Trading::Commodity(external_id = "ext_2",Notional=20000,MtM= -30,Si=0,Ei=2,BuySell='Sell',SubClass='Energy',commodity_type='Oil/Gas',Currency="USD", Counterparty = "cpty1")
tr3 = Trading::Commodity(external_id = "ext_3",Notional=10000,MtM= 100,Si=0,Ei=5,BuySell='Buy',SubClass='Metals',commodity_type='Silver',Currency="USD", Counterparty = "cpty1")
tr4 = Trading::IRDSwap(external_id = "ext_4",Notional=10000,MtM=30,Currency="USD",Si=0,Ei=10,BuySell='Buy', Counterparty = "cpty1")
tr5 = Trading::IRDSwap(external_id = "ext_5",Notional=10000,MtM=-20,Currency="USD",Si=0,Ei=4,BuySell='Sell', Counterparty = "cpty1")
tr6 = Trading::IRDSwaption(external_id = "ext_6",Notional=5000,MtM=50,Currency="EUR",Si=1,Ei=11,BuySell='Buy',OptionType='Put',UnderlyingPrice=0.06,StrikePrice=0.05, Counterparty = "cpty1")

trades= list(tr1,tr2,tr3,tr4,tr5,tr6)

  csa_raw = read.csv(system.file("extdata", "CSA_basel.csv", package = "SACCR"),header=TRUE,stringsAsFactors = FALSE)
  
  csas = list()
  for(i in 1:nrow(csa_raw))
  {
    csas[[i]] = Trading::CSA()
    csas[[i]]$PopulateViaCSV(csa_raw[i,])
  }

colls = list()
  coll_raw = read.csv(system.file("extdata", "coll_basel.csv", package = "SACCR"),header=TRUE,stringsAsFactors = FALSE)
  
  for(i in 1:nrow(coll_raw))
  {
    colls[[i]] = Trading::Collateral()
    colls[[i]]$PopulateViaCSV(coll_raw[i,])
  }

trees = runExampleCalcs(trades, csas, colls)

if(JSON==TRUE)
{
  requireNamespace("jsonlite")
  return(jsonlite::toJSON(as.list(trees[[1]])))
}
else
{
  return(trees[[1]])
}
}