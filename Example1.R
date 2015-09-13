# creating the trade objects and storing them in a list
tr1 = IRDSwap(Notional=10000,MtM=30,Currency="USD",Si=0,Ei=10,BuySell='Buy')
tr2 = IRDSwap(Notional=10000,MtM=-20,Currency="USD",Si=0,Ei=4,BuySell='Sell')
tr3 = IRDSwaption(Notional=5000,MtM=50,Currency="EUR",Si=1,Ei=11,BuySell='Sell',OptionType='Put',UnderlyingPrice=0.06,StrikePrice=0.05)

trades= list(tr1,tr2,tr3)

# calculating the add-on
Addon_Aggregate = CalcAddon(trades)

# calculating the RC and the V-c amount
rc_values = CalcRC(trades)

# calculating the PFE after multiplying the addon with a factor if V-C<0
PFE = CalcPFE(rc_values$V_C, Addon_Aggregate)

# calculating the Exposure-at-Default
EAD = CalcEAD(rc_values$RC,PFE)