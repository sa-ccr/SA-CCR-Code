######################################################################
# Create the base FX class
#
# This is used to represent the most basic agent in a simulation.
#' @include Trade.R
FX <- setRefClass("FX",
                   fields = list(ccyPair = "character"),
                   contains="Trade",
                   methods = list(
                     initialize = function(...){
                       SubClass<<-' '
                       callSuper(...,TradeGroup='FX')
                     }
                   ))

FXSwap <- setRefClass("FXSwap",
                       
                       contains="FX",
                       
                       methods = list(
                         initialize = function(...){
                           callSuper(...,TradeType='Swap')
                         }
                        
                       ))

