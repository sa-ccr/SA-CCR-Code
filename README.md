# SA-CCR

This repository contains a prototype implementation of the Basel III Standardized Approach for Counterparty Credit Risk Management. (you can view the regulation here: http://www.bis.org/publ/bcbs279.htm)


A few words about the code:

1. The trade structure is based on an Object Oriented Hierarchy where the Trade class  contains methods which apply for the all the trade types (for example the calculation of the supervisory delta, the maturity factor etc)
2. The calcAddon function performs all the necessary trade groupings and aggregations which apply for each trade type and returns the Addon amount.

If you want to use this code for commercial reasons or for any other queries please contact info@openriskcalculator.com
