#' Creates an IRD Swap 
#' 
#' Takes as inputs 
#' @param all the arguments that the parent class Trade uses
#' @return An object of type IRDSwap
#' @export
#' @include IRD.R
#' @examples
#' 
#' #returns 1.4*(60+500) = 784
#' tr1 = IRDSwap(Notional=10000,MtM=30,Currency="USD",Si=0,Ei=10,BuySell='Buy')

IRDSwap <- setRefClass("IRDSwap",
                       
                       contains="IRD",
                       
                       methods = list(
                         initialize = function(...){
                           callSuper(...,TradeType='Swap')
                         }
                       ))
