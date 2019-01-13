# SA-CCR

This repository contains a prototype implementation of the Basel III Standardized Approach for Counterparty Credit Risk Management. (you can view the regulation here: http://www.bis.org/publ/bcbs279.htm)


A few words about the code:

1. The trade structure is based on an Object Oriented Hierarchy where the Trade class  contains methods which apply for the all the trade types. For example, for the calculation of the supervisory delta, the supervisory duration etc a polymorphic method from the Trade class is being called. 
2. The calcAddon function performs all the necessary groupings and aggregations per netting set and returns the aggregate Addon amount.
3. The supervisory factors values are being read through a csv file.
4. All the examples of the regulatory paper have been implemented (ExampleIRD.R contains the code for the IRDs case etc)

If you want to become a contributor to the project or use this code for commercial purposes or for any other queries please contact us at info@openriskcalculator.com or visit our website www.openriskcalculator.com

# Donate 

[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=8HBDDB9MHXUTA)
