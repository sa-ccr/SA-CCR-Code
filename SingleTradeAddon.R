#' Calculates the addon information (including Adj notional, superv delta etc) for each trade
#' 
#' @title Calculates the addon information
#' @param trade A trade object
#' @param MF (Optional) The Maturity Factor based on the collateral agreement  
#' @return A list of addon information
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj
#' 
SingleTradeAddon = function(trade,MF)
{
  trade_results = list()
  trade_results$AdjNotional <- trade$CalcAdjNotional()
  
  if(missing(MF))
  {
    # calculate maturity factor
    trade_results$maturity_factor <- trade$CalcMaturityFactor()
  } else if(!missing(MF)&!("Future" %in% getClassDef(class(trade))@refSuperClasses))
  { trade_results$maturity_factor = MF}
  # if the trade is option based then for the delta calculation the volatility will be used
  if(length(trade$TradeType)!=0)
  {
    if (trade$TradeType=='Option')
    {
      superv <- LoadSupervisoryData()
      trade_results$volatility   <- superv$Supervisory_option_volatility[superv$Asset_Class==trade$TradeGroup&superv$SubClass==trade$SubClass]
      if(length(trade_results$volatility)==0)
      {
        if(superv$Asset_Class=='Commodity')
        {          
          if(trade$commodity_type=='Electricity')
          {trade_results$volatility = 1.5
          }else
          {trade_results$volatility = 0.7}
        }else if(superv$Asset_Class=='Other')
        {          trade_results$volatility = 1.5}
        else
        {          trade_results$volatility = 1.5}
      }
      trade_results$superv_delta <- trade$CalcSupervDelta(trade_results$volatility)
    }
    else
    {
      trade_results$superv_delta <- trade$CalcSupervDelta()
    }
  } else
  {
    trade_results$superv_delta <- trade$CalcSupervDelta()
  }
  # aggregating the add-on contribution for a specific hedging set
  trade_results$effective_notional <-  trade_results$superv_delta*trade_results$AdjNotional*trade_results$maturity_factor
  
  return(trade_results)
}