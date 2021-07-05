#' Calculates the  amount of the addon for each heding/nettting set
#' 
#' @title Calculates the Addon amount
#' @param trades_tree A tree structure with the input trades
#' @param MF (Optional) The Maturity Factor based on the collateral agreement
#' @param simplified (optional) When TRUE, the add-ons will be calculated as per the simplified SA-CCR
#' @param OEM (optional) When TRUE, the add-ons will be calculated as per the Original Exposure Method
#' @return The aggregate amount of the addon summed up for all the asset classes
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj
#' 
CalcAddon <- function(trades_tree, MF, simplified = FALSE, OEM = FALSE)  {
  ## function which will calculate the Add-On for all the trade classes
  
  superv <- LoadSupervisoryData()
  
  if(simplified)  superv[,'Correlation']=1
  
  asset_classes = trades_tree$`Asset Classes`$children
  asset_class_names = names(asset_classes)
  trades_tree$addon = 0
  # going through each trade class
  for (i in 1:length(asset_class_names))
  { 
    #picking up the trades belonging to this specific trade class
    asset_class_node = asset_classes[[asset_class_names[i]]]
    asset_class_node$addon = 0
    
    if(asset_class_names[i]=='FX')
    {
      ccypairs   <- asset_class_node$`Currency Pairs`$children
      # for the FX case the Hedging sets will be created based on the ccy pair
      ccypairs_names  <- names(ccypairs)
      ccypairs_addon <- array(data<-0,dim<-length(ccypairs))
      # going through all the ccy pairs found
      for (j  in 1:length(ccypairs_names))
      {
        
        ccypairs_trades  <- ccypairs[[ccypairs_names[j]]]$Trades$children
        ccypairs[[ccypairs_names[j]]]$add_on = 0
        ccypairs_trades_names = names(ccypairs_trades)
        
        # for each of the trade calculate the Adjusted Notional and their contribution to the addon of the hedging set
        for (l in 1:length(ccypairs_trades_names))
        {
          ccypairs_trade = ccypairs_trades[[ccypairs_trades_names[l]]]
          ccypairs_trade$exposure_details = SingleTradeAddon(ccypairs_trade$trade,MF)
          # aggregating the add-on contribution for a specific hedging set
          if(OEM)
          {   ccypairs[[ccypairs_names[j]]]$add_on <- ccypairs[[ccypairs_names[j]]]$add_on + ccypairs_trade$trade_details$Notional*ccypairs_trade$trade_details$Ei
          }else
          {   ccypairs[[ccypairs_names[j]]]$add_on <- ccypairs[[ccypairs_names[j]]]$add_on + ccypairs_trade$exposure_details$effective_notional          }
          

        }
        if(OEM)
        {  supervisory_factor = 0.04
        }else
        {
          factor_mult = CalculateFactorMult(ccypairs_names[j])
          # getting the supervisory factor
          supervisory_factor <- factor_mult*superv$Supervisory_factor[superv$Asset_Class==ccypairs_trade$trade$TradeGroup]
        }
        ccypairs[[ccypairs_names[j]]]$supervisory_factor = supervisory_factor
        ccypairs_addon[j] = supervisory_factor*ccypairs[[ccypairs_names[j]]]$add_on
        ccypairs[[ccypairs_names[j]]]$add_on = abs(ccypairs_addon[j])
      }
      asset_class_node$addon = sum(abs(ccypairs_addon))
      trades_tree$addon = trades_tree$addon + asset_class_node$addon
    }else if(asset_class_names[i]=='OtherExposure')
    {
      riskfactors   <- asset_class_node$`Other SubClasses`$children
      # for the FX case the Hedging sets will be created based on the ccy pair
      riskfactors_names  <- names(riskfactors)
      riskfactors_addon <- array(data<-0,dim<-length(riskfactors))
      # going through all the ccy pairs found
      for (j  in 1:length(riskfactors_names))
      {
        
        riskfactors_trades  <- riskfactors[[riskfactors_names[j]]]$Trades$children
        riskfactors[[riskfactors_names[j]]]$add_on = 0
        riskfactors_trades_names = names(riskfactors_trades)
        
        # for each of the trade calculate the Adjusted Notional and their contribution to the addon of the hedging set
        for (l in 1:length(riskfactors_trades_names))
        {
          riskfactors_trade = riskfactors_trades[[riskfactors_trades_names[l]]]
          riskfactors_trade$exposure_details = SingleTradeAddon(riskfactors_trade$trade,MF)
          # aggregating the add-on contribution for a specific hedging set
          riskfactors[[riskfactors_names[j]]]$add_on <- riskfactors[[riskfactors_names[j]]]$add_on + riskfactors_trade$exposure_details$effective_notional
          
        }
        factor_mult = CalculateFactorMult(riskfactors_names[j])
        # getting the supervisory factor
        supervisory_factor <- 0.08*factor_mult
        riskfactors[[riskfactors_names[j]]]$supervisory_factor = supervisory_factor
        riskfactors_addon[j] = supervisory_factor*riskfactors[[riskfactors_names[j]]]$add_on
        riskfactors[[riskfactors_names[j]]]$add_on = abs(riskfactors_addon[j])
      }
      asset_class_node$addon = sum(abs(riskfactors_addon))
      trades_tree$addon = trades_tree$addon + asset_class_node$addon
    }else if(asset_class_names[i]=='IRD')
    {
      
      # picking up the currencies found in the IRD trades which will be the first-level grouping applied 
      currencies   <-  asset_class_node$Currencies$children
      currencies_names  <- names(currencies)
      for (j  in 1:length(currencies_names))
      {
        
        currencies_buckets  <- currencies[[currencies_names[j]]]$Timebuckets$children
        
        currencies_buckets_names = names(currencies_buckets)
        
        for (k in 1:length(currencies_buckets_names))
        {
          currencies_buckets[[currencies_buckets_names[k]]]$effective_notional = 0
          #picking up all the trades belonging to a specific timebucket
          timebuckets_trades  <- currencies_buckets[[currencies_buckets_names[k]]]$Trades$children
          
          timebuckets_trades_names = names(timebuckets_trades)
          
          if(OEM)
          {  supervisory_factor = 0.005
          }else
          {
            factor_mult = CalculateFactorMult(currencies_names[j])
            supervisory_factor <- factor_mult*superv$Supervisory_factor[superv$Asset_Class==timebuckets_trades[[1]]$trade$TradeGroup&superv$SubClass==timebuckets_trades[[1]]$trade$SubClass]
          }
          
          currencies[[currencies_names[j]]]$supervisory_factor = supervisory_factor
          
          exotic_identifier = ''
          digital_eff_notional = 0
          for (l in 1:length(timebuckets_trades_names))
          {
            timebuckets_trade = timebuckets_trades[[timebuckets_trades_names[l]]]
            timebuckets_trade$exposure_details = SingleTradeAddon(timebuckets_trade$trade,MF)
            
            if(OEM)
            {   currencies_buckets[[currencies_buckets_names[k]]]$effective_notional = currencies_buckets[[currencies_buckets_names[k]]]$effective_notional + timebuckets_trade$trade_details$Notional*timebuckets_trade$trade_details$Ei
            }else
            {   currencies_buckets[[currencies_buckets_names[k]]]$effective_notional = currencies_buckets[[currencies_buckets_names[k]]]$effective_notional + timebuckets_trade$exposure_details$effective_notional          }

            if(!is.null(timebuckets_trade$trade$Exotic_Type)&length(timebuckets_trade$trade$Exotic_Type)!=0)
            {
              if(exotic_identifier!='')
              {  exotic_identifier = timebuckets_trade$trade$Exotic_Type
                 digital_eff_notional = digital_eff_notional + timebuckets_trade$exposure_details$effective_notional
              }else if(exotic_identifier==timebuckets_trade$trade$Exotic_Type)
              {
                digital_eff_notional = digital_eff_notional + timebuckets_trade$exposure_details$effective_notional
                
                excess_notional = abs(digital_eff_notional) - abs(timebuckets_trade$trade_details$Notional/supervisory_factor)
                currencies_buckets[[currencies_buckets_names[k]]]$effective_notional = currencies_buckets[[currencies_buckets_names[k]]]$effective_notional - max(excess_notional,0)
                exotic_identifier = ''
                digital_eff_notional = 0
              }
            }
          }

          # aggregating the add-on timebuckets recognizing correlation between each time bucket
          if(simplified)
            currencies[[currencies_names[j]]]$effective_notional  <- sum(abs(unlist(lapply(currencies_buckets,function(x) x$effective_notional))),na.rm = TRUE)
          else
            currencies[[currencies_names[j]]]$effective_notional = (sum(currencies_buckets[["1"]]$effective_notional^2,currencies_buckets[["2"]]$effective_notional^2,currencies_buckets[["3"]]$effective_notional^2,1.4*currencies_buckets[["2"]]$effective_notional*currencies_buckets[["3"]]$effective_notional,1.4*currencies_buckets[["2"]]$effective_notional*currencies_buckets[["1"]]$effective_notional,0.6*currencies_buckets[["2"]]$effective_notional*currencies_buckets[["1"]]$effective_notional,na.rm = TRUE))^0.5
          
          
          currencies[[currencies_names[j]]]$addon <- supervisory_factor*currencies[[currencies_names[j]]]$effective_notional
          
        }
        # adding up the addon of each currency after multiplying with the supervisory factor
        asset_class_node$addon <- asset_class_node$addon + currencies[[currencies_names[j]]]$addon
        
       
      }
      trades_tree$addon = trades_tree$addon + asset_class_node$addon
    }else  if(asset_class_names[i]=='Credit')
    {
      asset_class_node$addon = 0
      refEntities   <- asset_class_node$`Reference Entities`$children
      # for the FX case the Hedging sets will be created based on the ccy pair
      refEntities_names  <- names(refEntities)
      refEntities_addon <- array(data<-0,dim<-length(refEntities))
      
      # for the Credit case the Hedging sets will be created based on the reference entity
      supervisory_corel <- array(data<-0,dim<-length(refEntities_names))
      for (j  in 1:length(refEntities_names))
      {  
        refEntities_trades  <- refEntities[[refEntities_names[j]]]$Trades$children
        refEntities[[refEntities_names[j]]]$add_on = 0
        
        refEntities_trades_names = names(refEntities_trades)        
        for (k in 1:length(refEntities_trades_names))
        {
          refEntities_trade = refEntities_trades[[refEntities_trades_names[k]]]
          refEntities_trade$exposure_details = SingleTradeAddon(refEntities_trade$trade,MF)
          
          if(OEM)
          {  refEntities[[refEntities_names[j]]]$add_on <- refEntities[[refEntities_names[j]]]$add_on  + refEntities_trade$trade_details$Notional*refEntities_trade$trade_details$Ei
          }else
          {  refEntities[[refEntities_names[j]]]$add_on <- refEntities[[refEntities_names[j]]]$add_on  + refEntities_trade$exposure_details$effective_notional          }
        }
        
        AssetClass<-ifelse(class(refEntities_trade$trade)=='CDS','CreditSingle','CreditIndex')
        
        if(OEM)
        {  supervisory_factor = 0.06
        }else
        {  factor_mult = CalculateFactorMult(refEntities_names[j])
           supervisory_factor <- factor_mult*superv$Supervisory_factor[superv$Asset_Class==AssetClass&superv$SubClass==refEntities_trade$trade$SubClass]
        }
        
        refEntities[[refEntities_names[j]]]$add_on <- refEntities[[refEntities_names[j]]]$add_on*supervisory_factor
        supervisory_corel[j]  <- superv$Correlation[superv$Asset_Class==AssetClass&superv$SubClass==refEntities_trade$trade$SubClass]
        refEntities_addon[j] = refEntities[[refEntities_names[j]]]$add_on
        refEntities[[refEntities_names[j]]]$supervisory_corel = supervisory_corel[j]
        refEntities[[refEntities_names[j]]]$supervisory_factor = supervisory_factor
      }
      systematic_component     <- (sum(refEntities_addon*supervisory_corel))^2
      idiosynchratic_component <-  sum((rep(1,length(refEntities))-supervisory_corel^2)*refEntities_addon^2)
      asset_class_node$systematic_component = systematic_component
      asset_class_node$idiosynchratic_component = idiosynchratic_component
      asset_class_node$addon   <- sqrt(systematic_component + idiosynchratic_component)
      trades_tree$addon = trades_tree$addon + asset_class_node$addon
    }else   if(asset_class_names[i]=='Commodity')
    {
      AssetClass <-'Commodity'
      
      HedgingSets   <- asset_class_node$`Hedging Sets`$children
      
      HedgingSets_names  <- names(HedgingSets)
      
      # for the commodities case the Hedging sets will be created based on the commodity sector on a first
      # level (energy,metals etc) and on a second level based on the actual commodities (oil, silver etc)
      HedgingSets_addon = 0
      
      for (j  in 1:length(HedgingSets_names))
      {
        
        com_types  <- HedgingSets[[HedgingSets_names[j]]]$`Commodities Types`$children
        
        com_types_names = names(com_types)
        com_types_addon = 0
        com_types_addon_sq = 0
        for (k in 1:length(com_types_names))
        {
          com_types[[com_types_names[k]]]$addon = 0
          com_types[[com_types_names[k]]]$effective_notional = 0
          com_types_trades  <- com_types[[com_types_names[k]]]$Trades$children
          com_types_trades_names  <- names(com_types_trades)
          
          for (l in 1:length(com_types_trades_names))
          {
            com_types_trade = com_types_trades[[com_types_trades_names[l]]]
            com_types_trade$exposure_details = SingleTradeAddon(com_types_trade$trade,MF)
            
            if(OEM)
            {  com_types[[com_types_names[k]]]$addon <- com_types[[com_types_names[k]]]$addon + com_types_trade$exposure_details$effective_notional  + com_types_trade$trade_details$Notional*com_types_trade$trade_details$Ei
            }else
            {  com_types[[com_types_names[k]]]$addon <- abs(com_types[[com_types_names[k]]]$addon + com_types_trade$exposure_details$effective_notional)}
            
            com_types[[com_types_names[k]]]$effective_notional = com_types[[com_types_names[k]]]$effective_notional + com_types_trade$exposure_details$effective_notional
          }
          if(OEM)
            factor_mult = 1
          else
            factor_mult = CalculateFactorMult(HedgingSets_names[j])
          supervisory_factor <- factor_mult*ifelse(com_types_trade$trade$commodity_type=='Electricity',0.4,0.18)
          com_types[[com_types_names[k]]]$addon <- com_types[[com_types_names[k]]]$addon*supervisory_factor
          com_types[[com_types_names[k]]]$supervisory_factor = supervisory_factor
          com_types_addon = com_types_addon + com_types[[com_types_names[k]]]$addon
          com_types_addon_sq = com_types_addon_sq + com_types[[com_types_names[k]]]$addon^2
          #com_types[[com_types_names[k]]]$com_types_addon = com_types_addon
          
        }
        supervisory_corel     <- 0.4
        HedgingSets[[HedgingSets_names[j]]]$addon  <- sqrt((com_types_addon*supervisory_corel)^2 + (1-supervisory_corel^2)*com_types_addon_sq)
        HedgingSets[[HedgingSets_names[j]]]$supervisory_corel = supervisory_corel
        HedgingSets_addon = HedgingSets_addon + HedgingSets[[HedgingSets_names[j]]]$addon
      }
      asset_class_node$addon <- asset_class_node$addon + HedgingSets_addon
      trades_tree$addon = trades_tree$addon + asset_class_node$addon
    }else  if(asset_class_names[i]=='EQ')
    {
      asset_class_node$addon = 0
      refEntities   <- asset_class_node$`Reference Entities`$children
      # for the FX case the Hedging sets will be created based on the ccy pair
      refEntities_names  <- names(refEntities)
      refEntities_addon <- array(data<-0,dim<-length(refEntities))
      
      # for the Equity case the Hedging sets will be created based on the reference entity
      supervisory_corel <- array(data<-0,dim<-length(refEntities_names))
      for (j  in 1:length(refEntities_names))
      {  
        refEntities_trades  <- refEntities[[refEntities_names[j]]]$Trades$children
        refEntities[[refEntities_names[j]]]$add_on = 0
        
        refEntities_trades_names = names(refEntities_trades)        
        for (k in 1:length(refEntities_trades_names))
        {
          refEntities_trade = refEntities_trades[[refEntities_trades_names[k]]]
          refEntities_trade$exposure_details = SingleTradeAddon(refEntities_trade$trade,MF)
          if(OEM)
          {  refEntities[[refEntities_names[j]]]$add_on <- refEntities[[refEntities_names[j]]]$add_on  + refEntities_trade$exposure_details$effective_notional
          }else
          {  refEntities[[refEntities_names[j]]]$add_on <- refEntities[[refEntities_names[j]]]$add_on  + refEntities_trade$exposure_details$effective_notional          }
          
        }
        AssetClass<-asset_class_names[i]
        
        if(OEM)
        {  supervisory_factor = 0.32
        }else
        { factor_mult = CalculateFactorMult(refEntities_names[j])
          supervisory_factor <- factor_mult*superv$Supervisory_factor[superv$Asset_Class==AssetClass&superv$SubClass==refEntities_trade$trade$SubClass]}
        refEntities[[refEntities_names[j]]]$add_on <- refEntities[[refEntities_names[j]]]$add_on*supervisory_factor
        supervisory_corel[j]  <- superv$Correlation[superv$Asset_Class==AssetClass&superv$SubClass==refEntities_trade$trade$SubClass]
        refEntities_addon[j] = refEntities[[refEntities_names[j]]]$add_on
        refEntities[[refEntities_names[j]]]$supervisory_corel = supervisory_corel[j]
        refEntities[[refEntities_names[j]]]$supervisory_factor = supervisory_factor
      }
      systematic_component     <- (sum(refEntities_addon*supervisory_corel))^2
      idiosynchratic_component <-  sum((rep(1,length(refEntities))-supervisory_corel^2)*refEntities_addon^2)
      asset_class_node$addon   <- sqrt(systematic_component + idiosynchratic_component)
      asset_class_node$systematic_component = systematic_component
      asset_class_node$idiosynchratic_component = idiosynchratic_component
      trades_tree$addon = trades_tree$addon + asset_class_node$addon
    }  
  }
  sapply(trades_tree$leaves, function(x) x$trade = NULL)
  
  if(!missing(MF))
    trades_tree$maturity_factor = MF
  
  return(trades_tree)
}

