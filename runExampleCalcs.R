runExampleCalcs <-function(trades, csas, colls)
{
  if(length(trades)==length(unlist(lapply(trades,function(x) x$TradeType))))
  {
    if(all(unlist(lapply(trades,function(x) x$TradeType=='Option'&x$BuySell=='Sell'))))
    { 
      cat('All trades are sold options, EAD is zero')
      return(0)
    }
  }
  
  ext_trade_ids_temp = array()
  
  trade_trees = list()
  trades_temp = list()
  
  if(length(csas)!=0)
  {
    for(i in 1:(length(csas)+1))
    {
      if(i < (length(csas)+1))
      {
        csa_tradegroup = gsub("[']","",unlist(csas[[i]]$TradeGroups))
        csa_currency = gsub("[']","",unlist(csas[[i]]$Currency))
        
        trades_temp[[i]] = trades[sapply(trades, function(x)  x$TradeGroup %in% csa_tradegroup & x$Currency %in% csa_currency & x$Counterparty == csas[[i]]$Counterparty & !(x$external_id %in% ext_trade_ids_temp))]
        
        if(length(trades_temp[[i]])==0)
          next
        
        trades_tree = CreateTradeGraph(trades_temp[[i]]) 
        trade_ids = sapply(trades_temp[[i]], function(x) x$external_id)
        MF = csas[[i]]$CalcMF()
        ext_trade_ids_temp = c(ext_trade_ids_temp, trade_ids)
        # calculating the add-on
        trade_trees[[i]] = CalcAddon(trades_tree, MF)
        trade_trees[[i]]$maturity_factor = MF
      } else
      {
        trades_unmargined =  trades[sapply(trades, function(x)  !(x$external_id %in% ext_trade_ids_temp))]

        if(length(trades_unmargined)==0)
          next
        
        trades_temp[[i]] = trades[sapply(trades, function(x)  !(x$external_id %in% ext_trade_ids_temp))]
        trades_tree = CreateTradeGraph(trades_temp[[i]])
        trade_trees[[i]] = CalcAddon(trades_tree)
      }
    }
  } else
  {
    trades_tree = CreateTradeGraph(trades)
    trade_trees[[1]] = CalcAddon(trades_tree)
    trades_temp[[1]] = trades
  }
  
  for(i in 1:length(trade_trees))
  {
      # calculating the RC and the V-c amount
    if(i>length(csas))
    {
      trade_trees[[i]]$`Replacement Cost` <- CalcRC(trades_temp[[i]])
    }else
    {      trade_trees[[i]]$`Replacement Cost` <- CalcRC(trades_temp[[i]], csas[[i]], colls)   }
      
      # calculating the PFE after multiplying the addon with a factor if V-C<0
    trade_trees[[i]]$PFE <- CalcPFE(trade_trees[[i]]$`Replacement Cost`$V_C, trade_trees[[i]]$addon)
      
      # calculating the Exposure-at-Default
    trade_trees[[i]]$EAD <- CalcEAD(trade_trees[[i]]$`Replacement Cost`$RC,trade_trees[[i]]$PFE)
  }
  return(trade_trees)
}