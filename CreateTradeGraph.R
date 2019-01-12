#' Creates a tree-like structure describing the various hedging sets / risk factors that that the input trades can be broken into
#' 
#' @title Creates a tree-like structure of a list of trades
#' @param trades The full list of the Trade Objects
#' @return A tree structure based on hedging/netting sets and basis/volatility transactions
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' 

CreateTradeGraph <- function(trades)  {
  ## function which will calculate the Add-On for all the trade classes
  
  requireNamespace("data.tree")
  
  trade_classes <- unique(lapply(trades, function(x) x$TradeGroup))
  
  trade_classes_addon <- array(data<-0,dim<-length(trade_classes))
  
  head_node <- data.tree::Node$new("Trades Tree")
  
  asset_classes = head_node$AddChild("Asset Classes")
  
  trade_classes_tree=list()
  
  for(i in 1:length(trade_classes))
    trade_classes_tree[[i]] = asset_classes$AddChild(trade_classes[[i]])
  # going through each trade class
  
  for (i in 1:length(trade_classes))
  {  
    group_trades <- trades[sapply(trades, function(x) x$TradeGroup==trade_classes[i])]
  
    if(trade_classes[i]=="FX")
    {
      trade_classes_tree[[i]] = GroupTrades(group_trades, GroupFXTrades, trade_classes_tree[[i]])

    } else if(trade_classes[i]=="IRD")
    {
      # setting the time bucket for each of the trades
      lapply(group_trades, function(x) x$TimeBucket <- x$SetTimeBucket())
      trade_classes_tree[[i]] = GroupTrades(group_trades, GroupIRDTrades, trade_classes_tree[[i]])
      
    }else  if(trade_classes[i]=='Credit')
    {
      trade_classes_tree[[i]] = GroupTrades(group_trades, GroupCreditTrades, trade_classes_tree[[i]])
    }else   if(trade_classes[i]=='Commodity')
    {
      trade_classes_tree[[i]] = GroupTrades(group_trades, GroupCommTrades, trade_classes_tree[[i]])
    }else   if(trade_classes[i]=='EQ')
    {
      trade_classes_tree[[i]] = GroupTrades(group_trades, GroupEquityTrades, trade_classes_tree[[i]])
    }
    
    
  
}
  return(head_node)
}