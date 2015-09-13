tr1 = Commodity(Notional=10000,MtM=-50,Si=0,Ei=0.75,BuySell='Buy',SubClass='Energy',commodity_type='Oil/Gas')
tr2 = Commodity(Notional=20000,MtM=-30,Si=0,Ei=2,BuySell='Sell',SubClass='Energy',commodity_type='Oil/Gas')
tr3 = Commodity(Notional=10000,MtM=100,Si=0,Ei=5,BuySell='Buy',SubClass='Metals',commodity_type='Silver')

trades= list(tr1,tr2,tr3)

# calculating the add-on
Addon_Aggregate = CalcAddon(trades)

# calculating the RC and the V-c amount
rc_values = CalcRC(trades)

# calculating the PFE after multiplying the addon with a factor if V-C<0
PFE = CalcPFE(rc_values$V_C, Addon_Aggregate)

# calculating the Exposure-at-Default
EAD = CalcEAD(rc_values$RC,PFE)