# SA-CCR

This repository contains a prototype implementation of the Basel III Standardized Approach for Counterparty Credit Risk Management. (you can view the regulation here: http://www.bis.org/publ/bcbs279.htm)


A few words about the code:

1. The trade structure is based on an Object Oriented Hierarchy where the Trade class  contains methods which apply for the all the trade types. For example, for the calculation of the supervisory delta a polymorphic method from the Trade class is being called. 
2. The calcAddon function performs all the necessary groupings and aggregations which apply for each trade type and returns the Addon amount.
3. The supervisory factors values are being read through an excel file, therefore, the xlsx package is needed.
4. All the examples of the regulatory paper have been implemented (Example1.R contains the code for the IRDs case etc)
5. The margined workflow is not implemented yet.

If you want to use this code for commercial purposes or for any other queries please contact us at info@openriskcalculator.com
