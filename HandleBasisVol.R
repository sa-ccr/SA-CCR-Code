#' Receives a list of trades and splits them according to being basis, volatility or 'normal' transactions
#' @title Splits trades in being basis, volatility or 'normal' transactions
#' @param trades The full list of the Trade Objects
#' @return A list depicting which trade IDs fall under each hedging set.
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj
#'
HandleBasisVol <- function(trades)  {

  vol_hedging_sets   = array()
  basis_hedging_sets = array()
  vol_trades         = list()
  basis_swap_trades  = list()
  results = list()
  
  for (i in 1:length(trades))
  {
    if(is(trades[[i]],"Vol"))
      vol_trades[[length(vol_trades)+1]]=trades[[i]]
    else if(is(trades[[i]],"Swap"))
    {
      if(trades[[i]]$isBasisSwap())
        basis_swap_trades[length(basis_swap_trades)+1] = trades[[i]]
    }
  }
  if(length(basis_swap_trades)!=0)
  {
    basis_hedging_sets = unique(lapply(basis_swap_trades, function(x) paste(x$pay_leg_ref,x$rec_leg_ref)))
    
    basis_trade_ids = list()
    basis_trade_ids_all = array()
    
    for (i in 1:length(basis_hedging_sets))
    {
      split_pair = strsplit(basis_hedging_sets[[i]]," ")
      trades_temp <- basis_swap_trades[sapply(basis_swap_trades, function(x) x$pay_leg_ref==split_pair[[1]][1]&&x$rec_leg_ref==split_pair[[1]][2])]
      basis_trade_ids[[i]] = sapply(trades_temp, function(x) x$external_id)
      basis_trade_ids_all = c(basis_trade_ids_all,  basis_trade_ids[[i]])
    }
    
    basis_trade_ids_all = basis_trade_ids_all[!is.na(basis_trade_ids_all)]
    basis_hedging_sets = paste0("Basis_",basis_hedging_sets)
    
    results$trade_ids = c(results$trade_ids, basis_trade_ids)
    results$trade_ids_all = c(results$trade_ids_all, basis_trade_ids_all)
    results$hedging_sets = c(results$hedging_sets, basis_hedging_sets)
  }
  if(length(vol_trades)!=0)
  {
    vol_hedging_sets = unique(lapply(vol_trades, function(x) x$Underlying_Instrument))
    
    vol_trade_ids = list()
    vol_trade_ids_all = array()
    for (i in 1:length(vol_hedging_sets))
    {
      #picking up the trades belonging to this specific hedging set
      group_trades <- vol_trades[sapply(vol_trades, function(x) x$Underlying_Instrument==vol_hedging_sets[i])]
      vol_trade_ids[[i]] = sapply(group_trades, function(x) x$external_id)
      vol_trade_ids_all = c(vol_trade_ids_all, vol_trade_ids[[i]])
    }
    vol_trade_ids_all = vol_trade_ids_all[!is.na(vol_trade_ids_all)]
    vol_trade_ids = vol_trade_ids
    vol_hedging_sets = paste0("Vol_",vol_hedging_sets)
    
    results$trade_ids = c(results$trade_ids, vol_trade_ids)
    results$trade_ids_all = c(results$trade_ids_all, vol_trade_ids_all)
    results$hedging_sets = c(results$hedging_sets, vol_hedging_sets)
  }

  return(results)
}
