GroupTrades = function(group_trades, GroupingFunc, trade_classes_tree)
{
  results = HandleBasisVol(group_trades)
  hedging_super_set = "normal_trades"  
  if(length(results$hedging_sets)!=0)
    hedging_super_set = c(hedging_super_set,results$hedging_sets)
  
  for(h in 1:length(hedging_super_set))
  {
    if(hedging_super_set[h]=="normal_trades")
    {
      temp_trades = group_trades[sapply(group_trades, function(x) !(x$external_id %in% results$trade_ids_all))]
      
      if(length(temp_trades)==0)
        next
      
      trade_classes_tree = GroupingFunc(temp_trades, trade_classes_tree)
    }else
    {
      temp_trades = group_trades[sapply(group_trades, function(x) x$external_id %in% results$trade_ids[[h-1]])]
      trade_classes_tree = GroupingFunc(temp_trades, trade_classes_tree, hedging_super_set[h])
    }
    
  }
  return(trade_classes_tree)
}