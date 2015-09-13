tr1 = FXSwap(Notional=10000,MtM=30,ccyPair="EUR/USD",Si=0,Ei=10,BuySell='Buy')
tr2 = FXSwap(Notional=10000,MtM=-20,ccyPair="EUR/USD",Si=0,Ei=4,BuySell='Sell')
tr3 = FXSwap(Notional=5000,MtM=50,ccyPair="GBP/USD",Si=1,Ei=11,BuySell='Sell')

trades= list(tr1,tr2,tr3)

# calculating the add-on
Addon_Aggregate = CalcAddon(trades)

# calculating the RC and the V-c amount
rc_values = CalcRC(trades)

# calculating the PFE after multiplying the addon with a factor if V-C<0
PFE = CalcPFE(rc_values$V_C, Addon_Aggregate)

# calculating the Exposure-at-Default
EAD = CalcEAD(rc_values$RC,PFE)