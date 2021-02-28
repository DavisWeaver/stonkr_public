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

# Getting Started

1. To get started, you will need R and Rstudio installed on your computer as well as the package "devtools"
2. Next, install this package on your computer using devtools::install_github("github.com/DavisWeaver/stonkr"). 
3. To forecast share price for a given security, simply use:
```
renarin_short(ticker, zoom_in = FALSE)
```
4. For a full table containing the output (rather than a summary), specify zoom_in = TRUE. This is the data you will need to create visualizations. 
```
renarin_short(ticker, zoom_in = TRUE)
```
5. For more options / customization, type the following to view the documentation.
```
?renarin_short
```

6. For plotting, I have been using the following code: I plan on creating a pre-made function that does this in the near future but this should work for now. Make sure the ggplot2 package is installed. just replace df with the output from your renarin_short(ticker, zoom_in = TRUE) call.

```
ggplot(data = df, aes(x = date, y = fitted_actual)) +
    geom_point(aes(y = close)) + geom_line() + 
    labs(title = "some informative title") +
    scale_x_date(date_breaks = "4 weeks")
```
 
# Other notes

1. I don't recommend messing with the "vendor" parameter in renarin_short. That is mostly there so I can switch back and forth between the free yahoo data feed and the one I pay for. 

2. the default modeling parameters were chosen after quite a bit of iteration and testing. They are: look back (how many days into the past should we use for training), look ahead (how many days should we project), decay(passed to NNET), and lag (how many days should we use to predict the single next data point). 
3. I highly recommend messing with them just to get a sense of what they all do and how they impact the forecasting results. 
4. However, proceed with caution (well, even more caution) if you decide to tweak them for projections you plan to act on. If you do come across a recipe you think improves over the default settings and are comfortable sharing, I would love feedback!

5. Some of the code that I use to actually generate reports you may have seen currently resides outside of the package. My first priority is to rectify that so you have access to all the functionality that I do. Expect a few more functions to appear on this repository in the next week or so. These include the screener functions I described in my recent patreon update.

4. Most of the current functionality is through the renarin_short function. There are a few in-development areas that you probably don't want to touch. The create_reference document is an in-development group of functions for putting together a training dataset for other machine learning forecasting methods I have planned. I'm guessing they will break as currently written outside of my laptop. renarin_short_historic is similar to renarin_short but set up to evaluate forecasts in historical data. You can use it nearly the same way as renarin_short, except that it requires you to submit a dataframe containing all relevant historical price data for the security you specify. I used ~30 years of historical data from the sharadar equity prices on quandl. There are sadly no good historical data sources for stock prices that I could find for free. If you find one, please let me know so I can update the readme with instructions for how to get it. 

Feel free to email me at dweav94@gmail.com if you have any issues or questions!