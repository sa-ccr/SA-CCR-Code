GroupOtherTrades = function(group_trades, trade_classes_tree,hedging_set_name)
{
  
  # for the OtherExposure case the Hedging sets will be created based on the SubClass
  sub_classes   <- unique(lapply(group_trades, function(x) x$SubClass))
  
  if(!missing(hedging_set_name))
  {
    if(substr(hedging_set_name,1,4)=="Vol_"||substr(hedging_set_name,1,6)=="Basis_")
      temp_sub_classes = paste0(hedging_set_name,"_",sub_classes)
    
  }else
  { temp_sub_classes = sub_classes}
  
  sub_classes_addon <- array(data<-0,dim<-length(sub_classes))
  sub_classes_tree = list()
  
  if(length(trade_classes_tree[["Other SubClasses"]])==0)
  {  sub_classes_tree_name = trade_classes_tree$AddChild("Other SubClasses")
  }else
  {  sub_classes_tree_name = trade_classes_tree[["Other SubClasses"]]}
  
  # going through all the ccy pairs found
  for (j  in 1:length(sub_classes))
  {  
    # picking up all the trades for this ccy pair
    sub_classes_trades  <- group_trades[sapply(group_trades, function(x) (x$SubClass==temp_sub_classes[j]))]
    sub_classes_tree[[j]] = sub_classes_tree_name$AddChild(temp_sub_classes[[j]])
    
    trades_tree_name = sub_classes_tree[[j]]$AddChild("Trades")
    for (l in 1:length(sub_classes_trades))
    {
      tree_trade = trades_tree_name$AddChild(sub_classes_trades[[l]]$external_id)
      tree_trade$trade_details = Trading::GetTradeDetails(sub_classes_trades[[l]])
      tree_trade$trade = sub_classes_trades[[l]]
    }
  }
  return(trade_classes_tree)
}