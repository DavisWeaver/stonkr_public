# stonkr
Functions to forecast security price in US stock markets

# Welcome! 

Thank you so much for your support. I hope you find this resource as interesting and useful as I have. You'll find start up instructions further down but lets get some admin out of the way.

# Terms of Service

1. Your access to this github repository is for personal use only. 
2. You may not share this code with anyone without my express permission. 
3. This software comes with absolutely no warranty.
4. Davis Weaver and DW Forecasting are not liable for any gains or losses you might accrue trading using this software or related products. 

full TOS here: https://app.termly.io/document/terms-of-use-for-saas/24d989ae-26fd-4260-9ea7-3b3e691f79a8

# Setup

1. To get started, install this package on your computer using devtools::install_github("github.com/DavisWeaver/stonkr"). 

2. This package is currently only setup to work with a premium data feed from quandl https://www.quandl.com/databases/SEP/pricing/plans. 
One of my goals is to set it up to use the yahoo finance API for users such as yourself.
I had to use a premium feed during development because I was making so many API calls and need high-quality historic data. 
If you decide to use it "out of the box" then you will need to sign up for that data table and then edit the "get_api_key" function in create_reference to contain your api key from quandl. 
![image](https://user-images.githubusercontent.com/20169425/109398121-aaf7d500-7908-11eb-8fd6-a227c138483d.png)


3. Some of the code that I use to actually generate reports you may have seen currently resides outside of the package. My first priority is to rectify that so you have access to all the functionality that I do. Expect a few more functions to appear on this repository in the next week or so. 

4. Most of the current functionality is through the renarin_short function - take a look at the documentation for that function as well as renarin_short_historic if you want to test projections on historical data (again that requires you to actually get the historic data).

Feel free to email me at dweav94@gmail.com if you have any issues or questions!
