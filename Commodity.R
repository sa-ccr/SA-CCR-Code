# Create the base Commodity class
#
# This is used to represent the commodity asset class
#' @include Trade.R

Commodity <- setRefClass("Commodity",
                         fields = list(commodity_type      = 'character'
                         ),
                      contains="Trade",
                      methods = list(
                        initialize = function(...){
                          callSuper(...,TradeGroup='Commodity')} 
                      ))