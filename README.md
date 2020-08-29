# SA-CCR

Computes the Exposure-At-Default based on  the standardized approach
    of CRR2 (SA-CCR). The simplified version of SA-CCR has been included, as well as the OEM methodology.
	Multiple trade types of all the five major asset classes are being supported including the 'Other' Exposure and, given the inheritance-
    based structure of the application, the addition of further trade types
    is straightforward. The application returns a list of trees per Counterparty and CSA after
    automatically separating the trades based on the Counterparty, the CSAs, the hedging sets, the
    netting sets and the risk factors. The basis and volatility transactions are
    also identified and treated in specific hedging sets whereby the corresponding 
    penalty factors are applied. All the examples appearing on the
    regulatory papers (both for the margined and the un-margined workflow) have been
    implemented including the latest CRR2 developments.

If you want to become a contributor to this project, use this code for commercial purposes or for any other queries please contact us at info@openriskcalculator.com or visit our website www.openriskcalculator.com

# Donate 

If you have found this software of use, please consider supporting us by donating below:

[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=8HBDDB9MHXUTA)
