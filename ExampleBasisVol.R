#' Calculates the Exposure at Default for a trade set containing basis and volatility transactions
#' @title Basis+Volatility trades Example
#' @param JSON (optional) if TRUE it returns a json string 
#' @return The exposure at default
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj

ExampleBasisVol =function(JSON = FALSE)
{
  requireNamespace("Trading")
# creating the trade objects and storing them in a list
tr1 = Trading::IRDSwap(external_id = "ext_1",Notional=10000,MtM=30,Currency="USD",Si=0,Ei=10,BuySell='Buy',pay_leg_type='Float', pay_leg_ref  = "CDOR",
              pay_leg_tenor= "1M",rec_leg_type='Float',rec_leg_ref="CORRA",rec_leg_tenor="3M")
tr2 = Trading::CommSwap(external_id = "ext_2",Notional=10000,MtM=-20,Currency="USD",Si=0,Ei=4,BuySell='Sell',SubClass='Energy',commodity_type='Oil/Gas',pay_leg_type='Commodity', pay_leg_ref  = "Brent", rec_leg_type='Commodity', rec_leg_ref  = "Gas")
tr3 = Trading::IRDSwapVol(external_id = "ext_3",Notional=5000,MtM=50,Currency="EUR",Si=1,Ei=11,BuySell='Sell', Underlying_Instrument = "CDOR", vol_strike=0.2, annualization_factor=252)
tr4 = Trading::IRDSwap(external_id = "ext_4",Notional=10000,MtM=30,Currency="USD",Si=0,Ei=10,BuySell='Buy')

trades= list(tr1,tr2,tr3,tr4)

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