CalcRC <- function(trades)  {
  
  V_C = sum(sapply(trades, function(x) x$MtM))
  RC = max(V_C,0)
return(list("V_C"=V_C,"RC"=RC))
}