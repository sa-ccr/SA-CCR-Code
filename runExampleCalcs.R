runExampleCalcs <-function(trades, csas, colls, simplified = FALSE, OEM = FALSE, ignore_margin = FALSE)
{
  if(length(trades)==length(unlist(lapply(trades,function(x) x$TradeType))))
  {
    if(all(unlist(lapply(trades,function(x) x$TradeType=='Option'&toupper(x$BuySell)=='SELL'))))
    { 
      cat('All trades are sold options, EAD is zero')
      return(0)
    }
  }
  
  cpties = unique(unlist(lapply(trades,function(x) x$Counterparty)))
  
  if(length(cpties)>1)
  {
    trade_trees_all = list()
    
    for(cpty_counter in 1:length(cpties))
    {
      
      trades_cpty = trades[sapply(trades, function(x)  (x$Counterparty %in% cpties[cpty_counter]))]
      

      ext_trade_ids_temp = array()
      
      trade_trees = list()
      trades_temp = list()
      
      if(length(csas)!=0)
      {
        
        for(i in 1:(length(csas)+1))
        {
          if(i < (length(csas)+1))
          {
            if(cpties[cpty_counter] %in% unique(unlist(lapply(csas,function(x) x$Counterparty))))
            {
            csa_tradegroup = gsub("[']","",unlist(csas[[i]]$TradeGroups))
            csa_currency = gsub("[']","",unlist(csas[[i]]$Currency))
            
            trades_temp[[i]] = trades_cpty[sapply(trades_cpty, function(x)  x$TradeGroup %in% csa_tradegroup & x$Currency %in% csa_currency & x$Counterparty == csas[[i]]$Counterparty & !(x$external_id %in% ext_trade_ids_temp))]
            
            if(length(trades_temp[[i]])==0)
              next
            
            trades_tree = CreateTradeGraph(trades_temp[[i]]) 
            trade_ids = sapply(trades_temp[[i]], function(x) x$external_id)
            if(ignore_margin)
            {     MF =1
            }else {MF = csas[[i]]$CalcMF(simplified = simplified)}
            ext_trade_ids_temp = c(ext_trade_ids_temp, trade_ids)
            # calculating the add-on
            trade_trees[[i]] = CalcAddon(trades_tree, MF, simplified = simplified, OEM = OEM)
            trade_trees[[i]]$maturity_factor = MF
          } else
          {
            trades_unmargined =  trades_cpty[sapply(trades_cpty, function(x)  !(x$external_id %in% ext_trade_ids_temp))]
            
            if(length(trades_unmargined)==0)
              next
            
            trades_temp[[i]] = trades_cpty[sapply(trades_cpty, function(x)  !(x$external_id %in% ext_trade_ids_temp))]
            trades_tree = CreateTradeGraph(trades_temp[[i]])
            trade_trees[[i]] = CalcAddon(trades_tree, simplified = simplified, OEM = OEM)
          }
        }
      }
        }else
      {
        trades_tree = CreateTradeGraph(trades_cpty)
        trade_trees[[1]] = CalcAddon(trades_tree, simplified = simplified, OEM = OEM)
        trades_temp[[1]] = trades_cpty
      }
      
      for(i in 1:length(trade_trees))
      {
        # calculating the RC and the V-c amount
        if(i>length(csas))
        {
          trade_trees[[i]]$`Replacement Cost` <- CalcRC(trades_temp[[i]],simplified = simplified, ignore_margin = ignore_margin)
        }else
        {      trade_trees[[i]]$`Replacement Cost` <- CalcRC(trades_temp[[i]], csas[[i]], colls,simplified = simplified, ignore_margin = ignore_margin)   }
        
        # calculating the PFE after multiplying the addon with a factor if V-C<0
        trade_trees[[i]]$PFE <- CalcPFE(trade_trees[[i]]$`Replacement Cost`$V_C,trade_trees[[i]]$`Replacement Cost`$V, trade_trees[[i]]$addon, simplified = simplified)
        
        # calculating the Exposure-at-Default
        trade_trees[[i]]$EAD <- CalcEAD(trade_trees[[i]]$`Replacement Cost`$RC,trade_trees[[i]]$PFE)
        trades_tree_unmmargined = CreateTradeGraph(trades_temp[[i]])
        trades_tree_unmmargined= CalcAddon(trades_tree_unmmargined, simplified = simplified, OEM = OEM)
        trades_tree_unmmargined$`Replacement Cost` <- CalcRC(trades_temp[[i]],simplified = simplified, ignore_margin = ignore_margin)
        trades_tree_unmmargined$PFE <- CalcPFE(trades_tree_unmmargined$`Replacement Cost`$V_C,trades_tree_unmmargined$`Replacement Cost`$V, trades_tree_unmmargined$addon, simplified = simplified)
        trades_tree_unmmargined$EAD <- CalcEAD(trades_tree_unmmargined$`Replacement Cost`$RC,trades_tree_unmmargined$PFE)
        
        trade_trees[[i]]$EAD = min(trades_tree_unmmargined$EAD,trade_trees[[i]]$EAD)   
      }
      trade_trees_all[[cpty_counter]] = trade_trees
    }
    return(trade_trees_all)
    
  }else
  {
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
          MF = csas[[i]]$CalcMF(simplified = simplified)
          ext_trade_ids_temp = c(ext_trade_ids_temp, trade_ids)
          # calculating the add-on
          trade_trees[[i]] = CalcAddon(trades_tree, MF, simplified = simplified, OEM = OEM)
          trade_trees[[i]]$maturity_factor = MF
        } else
        {
          trades_unmargined =  trades[sapply(trades, function(x)  !(x$external_id %in% ext_trade_ids_temp))]
          
          if(length(trades_unmargined)==0)
            next
          
          trades_temp[[i]] = trades[sapply(trades, function(x)  !(x$external_id %in% ext_trade_ids_temp))]
          trades_tree = CreateTradeGraph(trades_temp[[i]])
          trade_trees[[i]] = CalcAddon(trades_tree, simplified = simplified, OEM = OEM)
        }
      }
    } else
    {
      trades_tree = CreateTradeGraph(trades)
      trade_trees[[1]] = CalcAddon(trades_tree, simplified = simplified, OEM = OEM)
      trades_temp[[1]] = trades
    }
    
    for(i in 1:length(trade_trees))
    {
      # calculating the RC and the V-c amount
      if(i>length(csas))
      {
        trade_trees[[i]]$`Replacement Cost` <- CalcRC(trades_temp[[i]],simplified = simplified, ignore_margin = ignore_margin)
      }else
      {      trade_trees[[i]]$`Replacement Cost` <- CalcRC(trades_temp[[i]], csas[[i]], colls,simplified = simplified, ignore_margin = ignore_margin)   }
      
      # calculating the PFE after multiplying the addon with a factor if V-C<0
      trade_trees[[i]]$PFE <- CalcPFE(trade_trees[[i]]$`Replacement Cost`$V_C,trade_trees[[i]]$`Replacement Cost`$V, trade_trees[[i]]$addon, simplified = simplified)
      
      # calculating the Exposure-at-Default
      trade_trees[[i]]$EAD <- CalcEAD(trade_trees[[i]]$`Replacement Cost`$RC,trade_trees[[i]]$PFE)
      
      trades_tree_unmmargined = CreateTradeGraph(trades_temp[[i]])
      trades_tree_unmmargined= CalcAddon(trades_tree_unmmargined, simplified = simplified, OEM = OEM)
      trades_tree_unmmargined$`Replacement Cost` <- CalcRC(trades_temp[[i]],simplified = simplified, ignore_margin = ignore_margin)
      trades_tree_unmmargined$PFE <- CalcPFE(trades_tree_unmmargined$`Replacement Cost`$V_C,trades_tree_unmmargined$`Replacement Cost`$V, trades_tree_unmmargined$addon, simplified = simplified)
      trades_tree_unmmargined$EAD <- CalcEAD(trades_tree_unmmargined$`Replacement Cost`$RC,trades_tree_unmmargined$PFE)
      
      trade_trees[[i]]$EAD = min(trades_tree_unmmargined$EAD,trade_trees[[i]]$EAD)   
    }
    return(trade_trees)
  }
}