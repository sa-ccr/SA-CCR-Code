#' Returns a tree structure depicting the add-on calculations on different hedging/netting sets
#' @title SA-CCR Calculator
#' @param trades_filename a .csv file containing the trades
#' @param csa_filename a .csv file containing CSAs
#' @param coll_filename a .csv file containing collaterals
#' @param JSON (optional) if TRUE it returns a json string
#' @param simplified (optional) if TRUE, the simplified SA-CCR is being calculated
#' @param OEM (optional) if TRUE, the Original Exposure Method is being calculated
#' @param export_results (optional) if TRUE, a csv with the exposure at the top level will be exported
#' @param ignore_margin (optional) if TRUE, the margin agreement workflow will be turned off
#' @return The relevant exposure trees
#' @export 
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj

SACCRCalculator = function(trades_filename, csa_filename, coll_filename, JSON = FALSE, simplified = FALSE, OEM = FALSE, export_results = FALSE, ignore_margin = FALSE)
{
  options(warn=-1)
  
  if(OEM) simplified = TRUE
  
  requireNamespace("Trading")
  trades = Trading::ParseTrades(trades_filename)
  trades = Trading::SelectDerivatives(trades)
  
  if(simplified) lapply(trades,function(x) x$simplified=TRUE)
  
  csas = list()
  if(!missing(csa_filename))
  {
    csa_raw = read.csv(csa_filename)
    
    
    for(i in 1:nrow(csa_raw))
    {
      csas[[i]] = Trading::CSA()
      csas[[i]]$PopulateViaCSV(csa_raw[i,])
    }
  }
  colls = list()
  if(!missing(coll_filename))
  {
    coll_raw = read.csv(coll_filename)
    
    for(i in 1:nrow(coll_raw))
    {
      colls[[i]] = Trading::Collateral()
      colls[[i]]$PopulateViaCSV(coll_raw[i,])
    }
  }
  trees = runExampleCalcs(trades, csas, colls, simplified = simplified, OEM = OEM, ignore_margin = ignore_margin)
  
  trees_length = length(trees)
  
  if(export_results)
  {
    to_be_exported = data.frame(CounterParty=rep(" ",trees_length),add_on = rep(0,trees_length),EAD = rep(0,trees_length),
                                PFE = rep(0,trees_length),Replacement_cost_VC= rep(0,trees_length),Replacement_cost = rep(0,trees_length))
    to_be_exported$CounterParty = as.character(to_be_exported$CounterParty)
    if(trees_length>1)
    {
      
      to_be_exported$CounterParty = as.character(to_be_exported$CounterParty)
      for(i in 1: trees_length)
      {
        to_be_exported$CounterParty[i] = as.character(trees[[i]][[1]]$name)
        to_be_exported$add_on[i] = trees[[i]][[1]]$addon
        to_be_exported$EAD[i] = trees[[i]][[1]]$EAD
        to_be_exported$PFE[i] = trees[[i]][[1]]$PFE
        to_be_exported$Replacement_cost_VC[i] = trees[[i]][[1]]$`Replacement Cost`$V_C
        to_be_exported$Replacement_cost[i] = trees[[i]][[1]]$`Replacement Cost`$RC
        #eval(listviewer::jsonedit(trees[[i]]))
      }
      
    }else
    {
      i=1
      to_be_exported$CounterParty[i] = as.character(trees[[i]]$name)
      to_be_exported$add_on[i] = trees[[i]]$addon
      to_be_exported$EAD[i] = trees[[i]]$EAD
      to_be_exported$PFE[i] = trees[[i]]$PFE
      to_be_exported$Replacement_cost_VC[i] = trees[[i]]$`Replacement Cost`$V_C
      to_be_exported$Replacement_cost[i] = trees[[i]]$`Replacement Cost`$RC
    }
    write.csv(to_be_exported,'exposure_per_counterparty.csv',row.names = FALSE)
  }

  
  if(trees_length>1)
  {
    head_node <- data.tree::Node$new("Counterparty Credit Risk Exposure Tree")
    
    for(i in 1:trees_length)
    {
      head_node$AddChildNode(trees[[i]][[1]])
    }
    trees = head_node
  }
  
  if(JSON==TRUE)
  {
    requireNamespace("jsonlite")
    if(trees_length>1)
    {
      return(jsonlite::toJSON(as.list(trees)))
    }else
    {
      return(jsonlite::toJSON(as.list(trees[[1]])))
    }
  }
  else
  {
    return(trees)
  }
}
