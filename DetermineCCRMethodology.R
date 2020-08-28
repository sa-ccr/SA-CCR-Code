#' Determines the CCR methodology that the institution is eligible to utilize.
#' The regulator allows the institutions to select less complicated methodologies when the derivatives trading business is negligible
#' @title Specifies the CCR methodology
#' @param trades_filename the file holding the trades of the portfolio
#' @param total_assets the total assets of the institution in mio EUR
#' @return The CCR methodology that the institution is eligible to utilize
#' @export
#' @author Tasos Grivas <info@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj

DetermineCCRMethodology <- function(trades_filename, total_assets)  {
  
  if(missing(trades_filename))
  { trades = read.csv(system.file("extdata", 'example_trades.csv', package = "Trading"),stringsAsFactors = FALSE,strip.white=TRUE)
  }else{
    trades = read.csv(trades_filename,stringsAsFactors = FALSE,strip.white=TRUE)  }
  
  trades = Trading::SelectDerivatives(trades)
  derivatives_mv = sum(abs(unlist(lapply(trades, function(x) x$MtM))))
  
  if(derivatives_mv/total_assets<0.05 & derivatives_mv<100000000)
  {   methodology='Original Exposure Method'
  }else if(derivatives_mv/total_assets<0.1 & derivatives_mv<300000000)
  {   methodology='Simplified SA-CCR'
  }else
  {   methodology='SA-CCR'}
  
  return(methodology)
}