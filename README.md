# stonkr
Functions to forecast security price in US stock markets

# Welcome! 

Thank you so much for your interest in this project. I hope you find this resource as interesting and useful as I have. You'll find start up instructions further down but lets get some admin out of the way.

# Terms of Service

1. This software comes with absolutely no warranty. 
2. We are not liable for any investing or trading decisions you make.
3. This is free, open-source, open-access software licensed under the GPL3 license.


# Getting Started

1. To get started, you will need R and Rstudio installed on your computer as well as the package "devtools"
2. Next, install this package on your computer using devtools::install_github("DavisWeaver/stonkr_public")
4. To forecast share price for a given security, simply use:
```
renarin_short(ticker, lag = 20, zoom_in = FALSE)
```
4. For a full table containing the output (rather than a summary), specify zoom_in = TRUE. This is the data you will need to create visualizations. 
```
renarin_short(ticker, lag = 20, zoom_in = TRUE)
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

2. the default modeling parameters are just examples - forecasts using them have been shown to be very poor at predicting price. 
3. I highly recommend messing with them just to get a sense of what they all do and how they impact the forecasting results. 

4. Most of the current functionality is through the renarin_short function. There are a few in-development areas that you probably don't want to touch. The create_reference document is an in-development group of functions for putting together a training dataset for other machine learning forecasting methods I have planned. I'm guessing they will break as currently written outside of my laptop. 
5. backest_short is a convenient function that calls renarin_short_historic repeatedly to evaluate the accuracy of model forecast parameters. Unfortunately, due to the issues with the yahoo finance API call number, this will only work if you pay for the SHaradar equity price tables on Quandl. - If you do decide to do that, simply put your API key into the "get_api_key" function in the quandl section and then set your vendor to quandl when making forecasts.

# Support

To try and preserve some balance with my main gig as a researcher, I am not able to be available all the time for support - if you have questions, email me at dweav94@gmail.com and I'll try to get back to you. 

# Whats next?

I have not found these single-stock forecasts to be very useful for guiding investment decisions. I am putting this package out as is mostly as a teaching/learning tool but its possible that someone smarter than me could tweak the parameters or extend the modeling framework to make useful inferences about future stock movement.

# Donate

You can find me on venmo www.venmo.com/Davis-Weaver
