# SA-CCR

R package implementing the Standardized Approach for the Counterparty Credit Risk Management (SA-CCR) of the Basel III Regulatory framework.

This repository contains a full implementation of the Basel III Standardized Approach for Counterparty Credit Risk Management. (you can view the regulation here: http://www.bis.org/publ/bcbs279.htm). Also, the follow up FAQ improvements have been implemented.

Computes the Exposure-At-Default based on standardized approach of the Basel III Regulatory framework (SA-CCR). Currently, trade types of all the five major asset classes have been created and, given the inheritance- based structure of the application, the addition of further trade types is straightforward. The application returns a list of trees (one per CSA) after automatically separating the trades based on the CSAs, the hedging sets, the netting sets and the risk factors. The basis and volatility transactions are also identified and treated in specific hedging sets whereby the corresponding penalty factors are applied. All the examples appearing on the regulatory paper (including the margined and the un-margined workflow) have been implemented.

If you want to become a contributor to this project, use this code for commercial purposes or for any other queries please contact us at info@openriskcalculator.com or visit our website www.openriskcalculator.com

# Donate 

If you have found this software of use, please consider supporting us by donating below:

[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=8HBDDB9MHXUTA)
