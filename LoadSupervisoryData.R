#' @description Loads the supervisory data (factors, correlation and option volatility)
#' for each Asset Class and SubClass
#' @title Supervisory Data Loading
#'  
#' @return A data frame with the required data
#' @export
#' @author Tasos Grivas <tasos@@openriskcalculator.com>
#' @references Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
#' http://data.europa.eu/eli/reg/2019/876/oj
LoadSupervisoryData <- function()  {
  
   # reading the excel file containing the supervisory factors
  superv <- read.csv(system.file("extdata", "supervisory_factors.csv", package = "SACCR"),header=TRUE,stringsAsFactors = FALSE)
  superv$Supervisory_factor = as.numeric(sub("%","",superv$Supervisory_factor))/100
  superv$Correlation = as.numeric(sub("%","",superv$Correlation))/100
  superv$Supervisory_option_volatility = as.numeric(sub("%","",superv$Supervisory_option_volatility))/100
  return(superv)
  
}