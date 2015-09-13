######################################################################
# Create the base Credit class
#
# This is used to represent the Credit asset class and it is the parent of the CreditSingle and CreditIndex subclasses
#' @include Trade.R

Credit <- setRefClass("Credit",
                   fields = list(RefEntity= "character"),
                   contains="Trade",
                   methods = list(
                     initialize = function(...){
                       callSuper(...,TradeGroup='Credit')
                     }
                   ))

CreditSingle <- setRefClass("CreditSingle",
                       
                       contains="Credit",
                       
                       methods = list(
                         initialize = function(...){
                           callSuper(...,TradeType='Single')
                         }
                       ))

CreditIndex <- setRefClass("CreditIndex",
                           contains="Credit",
                           
                           methods = list(
                             initialize = function(...){
                               callSuper(...,TradeType='Index')
                             }
                           ))
