GroupEquityTrades = function(group_trades, trade_classes_tree,hedging_set_name)
{
  # for the Credit case the Hedging sets will be created based on the reference entity
  refEntities   <- unique(lapply(group_trades, function(x) (if(length(x$Issuer)!= 0) x$Issuer)))
  refEntities = c(refEntities, unique(lapply(group_trades, function(x) (if(length(x$Underlying_Instrument)!= 0) x$Underlying_Instrument))))
  if(!missing(hedging_set_name))
  {
    if(substr(hedging_set_name,1,4)=="Vol_"||substr(hedging_set_name,1,6)=="Basis_")
      temp_refEntities = paste0(hedging_set_name,"_",refEntities)
    
  }else
  { temp_refEntities = refEntities
  temp_refEntities = temp_refEntities[sapply(temp_refEntities, function(x) !is.null(x))]
  }

refEntities_tree = list()
refEntities = refEntities[sapply(refEntities, function(x) !is.null(x))]

if(length(trade_classes_tree[["Reference Entities"]])==0)
{  refEntities_tree_name = trade_classes_tree$AddChild("Reference Entities")
}else
{  refEntities_tree_name = trade_classes_tree[["Reference Entities"]]}

for (j  in 1:length(refEntities))
{
  refEntities_trades  <- group_trades[sapply(group_trades, function(x) ((ifelse(length(x$Issuer== refEntities[[j]])!=0,x$Issuer == refEntities[[j]],FALSE) ) | (ifelse(length(x$Underlying_Instrument== refEntities[[j]])!=0, x$Underlying_Instrument == refEntities[[j]],FALSE))))]
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