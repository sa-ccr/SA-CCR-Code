CalcPFE <- function(V_C,Addon_Aggregate)  {
  
  if (V_C<0){
    PFE = min( 1, 0.05 + 0.95 * exp(V_C/(1.9*Addon_Aggregate)))*Addon_Aggregate
  }else  
  {PFE = Addon_Aggregate}
  
  return(PFE)
}