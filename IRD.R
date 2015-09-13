######################################################################
# Create the base IRD class
#
# This is used to represent the Interest Rate Derivatives product group and it is the parent of the 
# subclasses: IRDSwap and IRDSwaption
#' @include Trade.R

IRD <- setRefClass("IRD",
                   # the timebuckets grouping is only relevant for IRDs
                   fields = list(TimeBucket = "numeric"),
                   contains="Trade",
                   methods = list(
                     initialize = function(...){
                       SubClass<<-' '
                       callSuper(...,TradeGroup='IRD')
                     },
                     SetTimeBucket = function() {
                       ## sets the time bucket based on the maturity of the trade
                       if(Ei<1)         timebucket = 1;
                       if(Ei>=1&&Ei<=5) timebucket = 2;
                       if(Ei>5)         timebucket = 3;
                       return(timebucket)
                     }
                   ))






IRDSwaption <- setRefClass("IRDSwaption",
                       # Swaption contains unique fields relevant to the option's features   
                       fields = list(OptionType      = 'character',
                                     UnderlyingPrice = 'numeric',
                                     StrikePrice     = 'numeric'
                       ),
                       contains="IRD",
                       
                       methods = list(
                         initialize = function(...){
                            callSuper(...,TradeType='Option')
                         }
                       ))
