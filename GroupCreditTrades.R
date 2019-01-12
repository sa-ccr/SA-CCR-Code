GroupCreditTrades = function(group_trades, trade_classes_tree,hedging_set_name)
{
  # for the Credit case the Hedging sets will be created based on the reference entity
  refEntities   <- unique(lapply(group_trades, function(x) x$RefEntity))
  
  if(!missing(hedging_set_name))
  {
    if(substr(hedging_set_name,1,4)=="Vol_"||substr(hedging_set_name,1,6)=="Basis_")
      temp_refEntities = paste0(hedging_set_name,"_",refEntities)
    
  }else
  { temp_refEntities = refEntities}

refEntities_tree = list()
if(length(trade_classes_tree[["Reference Entities"]])==0)
{  refEntities_tree_name = trade_classes_tree$AddChild("Reference Entities")
}else
{  refEntities_tree_name = trade_classes_tree[["Reference Entities"]]}

for (j  in 1:length(refEntities))
{  
  refEntities_trades  <- group_trades[sapply(group_trades, function(x) (x$RefEntity == refEntities[j]))]
  refEntities_tree[[j]] = refEntities_tree_name$AddChild(temp_refEntities[[j]])
  
  trades_tree_name = refEntities_tree[[j]]$AddChild("Trades")
  
  for (k in 1:length(refEntities_trades))
  {
    tree_trade = trades_tree_name$AddChild(refEntities_trades[[k]]$external_id)
    tree_trade$trade_details = Trading::GetTradeDetails(refEntities_trades[[k]])
    tree_trade$trade = refEntities_trades[[k]]
  }
}
return(trade_classes_tree)
}