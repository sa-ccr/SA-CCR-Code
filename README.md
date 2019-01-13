# SA-CCR

This repository contains a prototype implementation of the Basel III Standardized Approach for Counterparty Credit Risk Management. (you can view the regulation here: http://www.bis.org/publ/bcbs279.htm)


A few words about the code:

1. The trade structure is based on an Object Oriented Hierarchy where the Trade class  contains methods which apply for the all the trade types. For example, for the calculation of the supervisory delta, the supervisory duration etc a polymorphic method from the Trade class is being called. 
2. The calcAddon function performs all the necessary groupings and aggregations per netting set and returns the aggregate Addon amount.
3. The supervisory factors values are being read through a csv file.
4. All the examples of the regulatory paper have been implemented (ExampleIRD.R contains the code for the IRDs case etc)

If you want to become a contributor to the project or use this code for commercial purposes or for any other queries please contact us at info@openriskcalculator.com or visit our website www.openriskcalculator.com

## Donate https://www.paypal.com/donate?token=Uh-hiCEConkQoPwNRxeXpBsDOWOpjJbGk5ypDEsYs90MvCcUGrGCBU-9DJrBP17ibVLGSG&country.x=GR&locale.x=GR

<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top">
<input type="hidden" name="cmd" value="_s-xclick" />
<input type="hidden" name="hosted_button_id" value="8HBDDB9MHXUTA" />
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" border="0" name="submit" title="PayPal - The safer, easier way to pay online!" alt="Donate with PayPal button" />
<img alt="" border="0" src="https://www.paypal.com/en_GR/i/scr/pixel.gif" width="1" height="1" />
</form>
