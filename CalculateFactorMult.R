CalculateFactorMult = function(hedging_set_name)
{
  if(substr(hedging_set_name,1,4)=="Vol_")
  {  factor_mult = 5
  }else if(substr(hedging_set_name,1,6)=="Basis_")
  {  factor_mult = 0.5    
  } else
  { factor_mult = 1}
  return(factor_mult)
}