---
title: "Mauritius_tourism_timeseries"
author: "Hpj"
date: "5/30/2020"
output:
  
  html_document: 
    keep_md: yes

---





```r
setwd("C:/Users/hansj/Dropbox/Fuqua/Tourism")
tou<-read_csv("tou_mru.csv")%>% #using tourist arrival data from mru
  rename(month=`Month of arrival`)%>% #changing name to easier option
  subset(month!="Year")#removing useless rows
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   `Month of arrival` = col_character(),
##   `2009` = col_number(),
##   `2010` = col_number(),
##   `2011` = col_number(),
##   `2012` = col_number(),
##   `2013` = col_number(),
##   `2014` = col_number(),
##   `2015` = col_number(),
##   `2016` = col_number(),
##   `2017` = col_number(),
##   `2018 1` = col_number()
## )
```





```r
tou_ts<-ts(as.numeric(as.matrix(tou))[13:132],frequency = 12,start=2009)
```

```
## Warning in is.data.frame(data): NAs introduced by coercion
```

```r
#Indicating that it data is monthly and srting series as from 2009
```



```r
autoplot(tou_ts)#shows clear seasonality as expected
```

![](Tourism_time_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
ggseasonplot(tou_ts,year.labels = TRUE)#exploring seasons
```

![](Tourism_time_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
gglagplot(tou_ts)#seeing relations betwen past periods and current, lagplot at period 12 is especially interesting showing a nearly one autocorrelation
```

![](Tourism_time_files/figure-html/unnamed-chunk-3-3.png)<!-- -->


**Autocorrelation**
Autocorelation is special to timeseries as it relates to the correlation between a value and the previous values before it for example r1 is the autocorrelation between Yt and Yt-1, r2 is the value between Yt and Yt-2
and so on, the ACF(autocorrelation function)Visually plots the rn
it will plot r1,r2,r3 etc (as a reminder r is correlation so it is bounded -1 to 1)


![](Tourism_time_files/figure-html/pressure-1.png)<!-- -->


As expected r12 has a high value as the data is seasonal on a yearly basis.
Touristic arrival this year at a specific month is highly correlated with touristic arrival during the same month last year.
It is also the case of r24,though it starts to decrease as 24 periods in btw makes a diff.

Note that when there is a trend(upward or downward) in a data, autocorelation is always positive.
Think about it,data if jan 2017 will be related to august 2018 even marginally and tourism is going up, then "jan tourist going up is related to aug tourist going up".
Same if tourism is down and "jan is going down and aug down" they are going same direction thus positive correlation thus alwyas a positive correlation even if it is a very low one.

Somewhat surprisingly in our data r6 is 0 and r18 is negative.

The blue lines represent the 95% confidence interval.
Confidence intervals represents the confidence with which a population mean(or true value) will be within certain bounds. A 95% confidence interval will mean that we are 95% confident that the true value is found within the bounds.

In the case of the autocorrelation diagraom, the bounds represent where we can say with 95% confidence the true autocorelation value lies. But peaks that go above these peaks represent autocorrelation that are significant

**Naive predictions**

A simple yet somehow relatively successful prediction method for time series is by using "naive" methods. These can be split in 3

1)Naive prediction: assumes that next values will same as last. Quite useful in predicting stock.

2)Seasonal naive: assumes that next values will be same as during last similar season

3)Drift: Which assumes that the next value at n, will be an extension of the change from t0 and tn-1


```r
autoplot(tou_ts)+autolayer(rwf(tou_ts,drift = TRUE,h=24),series = "Drift",PI=FALSE)+autolayer(snaive(tou_ts,h=24),series="Season.Naive",PI=FALSE)+autolayer(rwf(tou_ts,h=24),series="Naive",PI=FALSE)#Pi=false removes conf interval from plot which would hide the 3
```

![](Tourism_time_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


**Standardization**
It is possible that some months just have more days than others and due to that have higher number of tourists, so to adjust for that,we divide by the number of days in the month

```r
autoplot(tou_ts)#without "standardization"
```

![](Tourism_time_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
autoplot(tou_ts/monthdays(tou_ts))
```

![](Tourism_time_files/figure-html/unnamed-chunk-5-2.png)<!-- -->


Albeit very slightly, the standardization does smoothen the graph, thus providing more accurate representation of the numbers.
Hence predictions made on the standardization should be more accurate.


**Prediction accuracy**

To ensure how well our models predict future performance, we have a couple of options.
Given the continuous nature of our data we can use measures such as RMSE and MSE which will evaluate the model based on the difference between actual value and predicted

Let's check the accuracy of our "simple" models using the rmse(because the unit for rmse is the same as for our variables,which isnt the case for mse).

For that we will use data from 2009-2017 to predict 2018


```r
adj_days<-round(tou_ts/monthdays(tou_ts),0)#adjusting for number of days
adj_days_2017<-window(adj_days,start=2009,end=c(2017,12))#sampling 2009-Dec 2017

autoplot(adj_days)+autolayer(snaive(adj_days_2017,h=12),series="Season naive")+
  autolayer(rwf(adj_days_2017,drift = TRUE,h=12),PI=FALSE,series="Drift") +
  xlab("Year") + ylab("Arrivals") +
  ggtitle("Forecasts for tourism arrivals") 
```

![](Tourism_time_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


Now we will check the accuracy on 2018 


```r
accuracy(rwf(adj_days_2017,drift = TRUE,h=12),h=12)#h= 12 for a 12 months period
```

```
##                        ME     RMSE      MAE      MPE     MAPE     MASE
## Training set -4.67601e-14 501.5261 404.9865 -1.73613 14.09725 2.162691
##                    ACF1
## Training set -0.3356098
```

```r
accuracy(snaive(adj_days_2017,h=12),h=12)
```

```
##                    ME     RMSE      MAE     MPE     MAPE MASE      ACF1
## Training set 161.1562 236.3738 187.2604 5.15616 6.128069    1 0.2584712
```


We can see that the seasonal naive does a much better job at predicting the arrivals in 2018 than the drift. Which makes sense when we look at the complete dataset, the growth between 2017-2018 wasnt representative of the growth since 2009.


**Time-series Regression**

In the case where there is a linear relation in the time series between our predictors and our forecast, then we can apply a time series linear regression to the time series.

More than this asumption of linearity, we also make a couple of other assumptions related to linear regression

>Heterosdacity: That the variance of errors doesnt change across predictors, that is that as predictor increase, variance of error shouldnt increase or decrease

>Normality: That errors are normally ditributed

>Multicollinearity: Independent variable shouldnt be corelated

>Independance of error assumption: residuals are independent from each other. Meaning an error term at one point in time is not related to that at another point else it means we are missing some information

> Mean of residuals: That the mean of residual is zero(or very close) as it being more than zero would mean we have a bias in one direction and we are systematically falsely predicting in one direction


The question is in our data ,what can we use as predictor? There is probably a lot of variables that affect tourist arrivals but in our data we have some time series specific variables:

1) Trend: Our data has a clear upward trend, this can be a predictor in our forecast (a bit how the drift method does it)

2)Months: There are clearly periods that have more tourists than others as seen in our seasonality graph showing months like December having more arrivals, and months like June less. Thus it makes sense to have them as predictors.
Note that we will have, as in any categorical predictor, n-1 predictors therefore in our case 11. n-1 as one(JAN) is used for our base case.

If we use these 2 above predictors then things will look like this:

arrival=trend+month1+month2+month3...month11

predicted arrival=Bt(trend)+B1(month1)+B2(month2)....B11(month11)+B0+error


```r
fit_arrival<-tslm(adj_days~trend+season)#run model
summary(fit_arrival)#see results
```

```
## 
## Call:
## tslm(formula = adj_days ~ trend + season)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -348.56 -127.08   -4.52  143.53  429.60 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2604.9588    65.4999  39.770  < 2e-16 ***
## trend          13.4935     0.4983  27.081  < 2e-16 ***
## season2      -277.4935    84.1352  -3.298 0.001322 ** 
## season3      -328.3870    84.1397  -3.903 0.000167 ***
## season4      -537.7804    84.1470  -6.391 4.40e-09 ***
## season5      -793.9739    84.1574  -9.434 9.74e-16 ***
## season6     -1332.2674    84.1706 -15.828  < 2e-16 ***
## season7      -542.8609    84.1869  -6.448 3.35e-09 ***
## season8      -838.2543    84.2060  -9.955  < 2e-16 ***
## season9      -861.0478    84.2281 -10.223  < 2e-16 ***
## season10      -95.9413    84.2532  -1.139 0.257361    
## season11     -174.4348    84.2812  -2.070 0.040891 *  
## season12      628.3718    84.3121   7.453 2.49e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 188.1 on 107 degrees of freedom
## Multiple R-squared:  0.9364,	Adjusted R-squared:  0.9293 
## F-statistic: 131.3 on 12 and 107 DF,  p-value: < 2.2e-16
```


These are interesting results. Note that trend and season are not elements named as such in the dataset, the function tslm captures the trend and labels our months as seasons.

So the result is that for every one unit increase in trend, tourist arrival increase by 13 on average per day (yeah not by 13.49 tourists).
As for season, the base season(s1 in this) is January.
Meaning that most months bring on average a certain number of arrivals less than January for each day(as much as 1332 less for June-season 6). However December(season12) brings on average 628 tourists more on average per day. 
Which all makes sense when you know the seasonality of tourists in Mauritius.
Special attention should be given to season 10 and 11, they bring less tourist than January but not by a lot. This doesnt give the whole picture as in recent years, these 2 months, especially october has sytematically brought more tourists than January.(Also note the non-significance of October, I address this below)

Let's check this by using a shorter window for our model

```r
fit_arrival_2013<-tslm(window(adj_days,start=2013)~trend+season)
summary(fit_arrival_2013)
```

```
## 
## Call:
## tslm(formula = window(adj_days, start = 2013) ~ trend + season)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -244.47  -74.71   -1.69   82.28  337.66 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2909.1113    60.8997  47.769  < 2e-16 ***
## trend          20.0018     0.7861  25.443  < 2e-16 ***
## season2      -162.1685    78.9323  -2.055  0.04436 *  
## season3      -263.6702    78.9441  -3.340  0.00146 ** 
## season4      -462.1720    78.9636  -5.853 2.27e-07 ***
## season5      -744.6738    78.9910  -9.427 2.23e-13 ***
## season6     -1322.1756    79.0262 -16.731  < 2e-16 ***
## season7      -469.8440    79.0692  -5.942 1.61e-07 ***
## season8      -734.8458    79.1200  -9.288 3.80e-13 ***
## season9      -792.5143    79.1786 -10.009 2.48e-14 ***
## season10       40.1506    79.2449   0.507  0.61428    
## season11      -84.6845    79.3190  -1.068  0.29003    
## season12      724.3137    79.4007   9.122 7.15e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 136.7 on 59 degrees of freedom
## Multiple R-squared:  0.9664,	Adjusted R-squared:  0.9596 
## F-statistic: 141.5 on 12 and 59 DF,  p-value: < 2.2e-16
```


Indeed October(though not November) shows higher tourist arrival than January. Just a point of the importance of knowing the data and not blindly trust the model.

As for the non-significance, what it means in this case, is that the mean arrival in October is not an unsual number when compared to the mean of our whole dataset(less than 2standard dev away from the sample mean).

Intuitively, given that arrivals in October have varied a lot(less than arrivals than Jan initially,then more) we can imagine that it would have a mean that will be quite in the middle,thus "closer" to the overall mean of our sample. Infact if we were to run the model on an even smaller window where October always has a high amount of tourist, it becomes a significant predictor.

###Predicting 2019 using regression
When using regression to forecast time series, there are 3 types that should be considered.

-ex-ante: Uses predictors that we have now to predict the future. For example if price of air tickets is a predictor and out data ends at Dec 2018(and let's assume we are in 2018...yes when we could still go out), in an ex-ante model where we try to predict arrivals in 2019, we should use whatever estimation of the price of airticket fro 2019, that we have at date, that is at Dec 2018. 
These estimates can be either obtained through what experts say, through our "naive" methods or through a regression derived method.Now if the actual prices are static and printed in dec 2018, we can use those

-ex-post-ante: Uses obeserved predictors. That is, taking the example of ticket price, we should take the actual ticket price in 2019 to predict tourist arrival.This of course may involve that we are running the model in the actual period we are trying to predict. Note that only observed predictors are used, not actual obeserved value. While not very useful as a forecasting method is interesting to understand the forecasting model or to compare with an ex-ante model and observe differences.

In our goal of predicting a case of tourist in 2020 with a reopening of frontiers in september 2020, we only have predictors in an ex-ante fashion up to end of 2018. From there we have to map out 2019 tourism, which we can do using a regression with above significant predictors(note that since both trend and seasons are based of calendar, we do know them in advance and in this case ex-ante and ex-post are same).

But then in 2020, things get more complicated because we have to factor in the covid, there we will use a combination of ex-post ante(we know that currently frontier are closed thus zero ppl coming) and scenario based forecasting.

Arrival numbers till now wouldnt be good predictors for the rest of the year given they dont take in consideration the whole covid picture, which can vary wildly.
So in this case we will refer to scenario based planning to use regression to predict arrivals in the future:

-Scenario based forecasting: It involves applying specific scenarios to forecasting, for example an increase in tourist traffic of X%


```r
#mapping 2019 tourism
# we will keep October despite it not being significant as it is part of the increasing trend of recent years and to highlight this we will use more recent years
arri_model<-tslm(window(adj_days,start=2012)~trend+season)
summary(arri_model)
```

```
## 
## Call:
## tslm(formula = window(adj_days, start = 2012) ~ trend + season)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -266.31  -88.79    0.24   76.28  365.79 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2803.9306    61.5775  45.535  < 2e-16 ***
## trend          18.2837     0.6767  27.020  < 2e-16 ***
## season2      -204.7123    79.5634  -2.573 0.012175 *  
## season3      -297.4246    79.5721  -3.738 0.000372 ***
## season4      -478.1369    79.5865  -6.008 7.30e-08 ***
## season5      -769.2778    79.6066  -9.663 1.41e-14 ***
## season6     -1334.2758    79.6325 -16.755  < 2e-16 ***
## season7      -513.9881    79.6641  -6.452 1.17e-08 ***
## season8      -789.5575    79.7014  -9.906 5.08e-15 ***
## season9      -827.8413    79.7445 -10.381 6.99e-16 ***
## season10      -16.5536    79.7933  -0.207 0.836248    
## season11     -137.5516    79.8478  -1.723 0.089301 .  
## season12      685.0218    79.9080   8.573 1.45e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 148.8 on 71 degrees of freedom
## Multiple R-squared:  0.9607,	Adjusted R-squared:  0.954 
## F-statistic: 144.5 on 12 and 71 DF,  p-value: < 2.2e-16
```

```r
arri_model_2019<-forecast(arri_model,h=12)#without h=12 prediction stops at oct
autoplot(arri_model_2019)+ggtitle("Forecast for 2019")+xlab("year")+ylab("arrivals")
```

![](Tourism_time_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
Ok so now we have our predictions for 2019(who seem intutively very close to what we would expect).


However regression is one way of predicting time series, a more frequently used and often more robust set of models are the ARIMA models.


# ARIMA Models

Arima models are models used for time series that use either autoregression or moving averages to predict future values.
In fact ARIMA stands for Auto Regressive Integrated Moving Average.

**Auto-regressive** Refers to mode that use previous values to predict the future. You can picture an ARIMA model as a multiple regression with previous time periods as the predictors:

Y=b0+b1(Y-1)+b2(Y-2)+b3(Y-3)...+bn(Y-n)+Et
where E is the error at Y

An AR model doesnt neccessarily take into account all lags. An example would be an event that is dependent only on the past day, not on the day before the past day.

Such a model would be called A(1) and would be written as such

Y=b0+b1(Y-1)+Et
in the same way Y-1 can be broken down a
Y-1=b0+b1(Y-2)+Ey-1 and so on.

Note that b1 is the same coefficient that is carried over in an AR(1) model.
It can be described as this value by which a previous day affect the current day.

An AR(2) model will be more complicated as it would involve the following

Y=b0+b1(y-1)+b2(y-2)+Et
with both y-1 and y-2 being of product of the two days preceeding them.
with b2 being the vlaue by which the data from two days ago affect today's.

1 or 2 used above is refered to p, thus an AR model is described as AR(p)
Of course the more past periods affect a current one, the greater p and in consequence the more coefficients youll have...which can make quite a complex model easilly(imagine a model where every past day in one year affects today!!!!)

Luckily R facilitates this process for us.

**MA** 

MA refers to models that used, instead of autocorrelation, past errors as input in their model.

The resulting model is something as folows

Y=b0+b1(Et-1)+b2(E-2)+b3(Et-3)...+bn(Et-n)+Et
where Et is the error at Y and et-1 is the error at time t-1 and so on...

As with AR models, MA models have a notation in this case MA(q) where q is the lag of errors taken in consideration


So we have AR we have MA, we still need the I in the middle.
It stands for integrated


**Integrated** refers to an important aspect. It refers to the aspect that reduces trend and seasonality. This implies that ARIMA model are not meant to be applied to data that is not stationary initially. In fact the equivalent for stationary data would be called ARMA model

So what is stationarity to start?
 We say that a time series is staionary when there is no variation in mean and Variance over a period over time. This would mean that timeseries that have a trend and/or a seasonal aspects are (often)not Stationary.
 A time series that has a trend is one where the mean isnt the same for all time periods and a time series that has seasonality is one where the Variance isnt the same across periods. Often times both are present and this is where the integrated part comes.
 
 One way to get rid of trend/seasonality or to reduce is to transform the variables. In our case we already exprimenting with some transformation before, let's see how this affects mean and variance.

```r
#first see our normal data
autoplot(tou_ts)+ggtitle("untransformed data")# trned and seasonality high
```

![](Tourism_time_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#let's try adjusting for number of days per month
autoplot(tou_ts/monthdays(tou_ts))+ggtitle("Adjusted month data")
```

![](Tourism_time_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
#adjusting for days does help decrease variance

#let us try using a log transformation
autoplot(log(tou_ts/monthdays(tou_ts)))+ggtitle("Log tranformed month data")
```

![](Tourism_time_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
# doesnt really make a difference
```
 
 After transforming the data, another mean we can use to convert the data to a stationary is using differencing.
 
 Differencing is finding the difference between two values, between Y and y-1 for example. This difference is usually must more stationary. Differencing is an essential part of ARIMA. In fact as with AR and MA, I has an ssociated valuer I(d). 
 D is the order differencing meaning how many time do we find values between values. The above mentioned ARMA model for example would have I(0)
 Whereas a model where we try to find difference between values once will be I(1).
 
 let's try to do so in our data adjusted by days(remember we did this prior to regression)

```r
diff_adj<-diff(adj_days)
autoplot(diff_adj)+ggtitle("Differenced-Adjusted month data")
```

![](Tourism_time_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
 
 We see that by differencing once, we "kill" the trend and have a flat series, we do however still have a stong seasonality element.
 let's confirm that

```r
ggAcf(diff_adj)
```

![](Tourism_time_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
 The seasonality is reflected in the autocorelations namely at lag-6 and lag-12.
 
 To remove seasonality, we can do one of two things, either conduct another normal difference 2nd order differencing which means finding the difference between each point a second time I(2) or instead of normal differencing we can find the differencee between points in the same seasons this is called seasonal differencing I(1)s.

Given that we already knew that our data has a strong seasonality aspect, let's try instead to use seasonal differencing
 
Given our data's seasonality, we will do our second order differencing as seasonal differencing.


```r
s.diff_adj<-diff(tou_ts/monthdays(tou_ts),6)
autoplot(s.diff_adj)+ggtitle("seasonal differenced(6)-Adjusted month data")
```

![](Tourism_time_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
ggAcf(s.diff_adj)
```

![](Tourism_time_files/figure-html/unnamed-chunk-14-2.png)<!-- -->
 We accounted for a sesonality at lag-6 and we still have strong seasonality.
 
This is also reflected in the high autocorrelation that we can show on the ACF plot(see correlation going out of confidence bounds).
This autocorrelation is something we can checked in another way than through ACF and this is by using the Ljung-Box test.
 
 Basically :Ljung test conduct hypothesis testing with Ho being that autocorrelations in our data is just due to chance and H1 being that it is less likely to be due to chance. As in most p-value hypothesis testing we use a threshold of 0.05.
 
 However in this case we want(hope) that our H0 is true, that is that the likelihood that autocorrelation is due to randomness is high.THus we look for p-values above 0.05.
 
 Let's apply the test to our 2nd order seasonal differencing data
 

```r
Box.test(s.diff_adj,type="Ljung-Box")
```

```
## 
## 	Box-Ljung test
## 
## data:  s.diff_adj
## X-squared = 31.785, df = 1, p-value = 1.722e-08
```
 
Yeah...as expected it states that the chance of autocorrrelation being due to chance is very very low.

So let's try a 2nd order differencing using another seasonal differencing I(2)s, this time taking the lags at 12.

```r
autoplot(diff(s.diff_adj,12))+ggtitle("3rd differenced(12-6)-Adjusted month data")
```

![](Tourism_time_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
ggAcf(diff(s.diff_adj,12))
```

![](Tourism_time_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

```r
Box.test((diff(s.diff_adj,12)),type="Ljung-Box")
```

```
## 
## 	Box-Ljung test
## 
## data:  (diff(s.diff_adj, 12))
## X-squared = 0.11334, df = 1, p-value = 0.7364
```
BINGO! A second order seasonal diffference makes the data stationary as seen by the correaltions that are within our confidence bounds. At 12 the autocorrealtion does go slightly above the bounds, but the high p-value on the ljung-box test, confirms that this is more likely not a big deal
 

**Combining AR and MA**
While we can use either AR or MA, we can also use a combinaison of auto regression of past values and "moving averages" of past errors to forecast on our time-series.

Armia models are usually expressed as ARIMA(p,d,q) where

p is for the number of autocorrelation in the model
d is for the lvel of differencing(first order or more)
q is the number of lagged forecast errors(resulted to the MA).

For example if we go for a model that is an AR(2) and has a first order differention and we use past errors from the lag-1 then we it will be ARIMA(2,1,1).

Some interesting models written in this fashion are
1)ARIMA(1,0,0) which is basically AR(1) 
2)ARIMA(0,0,1) is basically MA(1)
3)ARIMA(0,1,0) is a first order differenced model I(1) and our series expresses a random walk where the next value can go in any direction
4)ARIMA(0,0,0) means our model doesnt consider neither of the 3 parameters. It means no variables are corelated,that is no variable can allow us to predict the series. it can be interpreted as all noise. 

In some cases (like in our case) there is a need to account for seasonality in our forecast then the model will be along the lines of ARIMA(p,d,q)(P,D,Q)m
where

(P,D,Q) stands for the autocorrelation,differencing and errors on the seasonal level (that is compared to the same season at a different point)
and m stands for the season frequency.

Let us try predicting 2019 data using an ARIMA model.

**Predicting 2019 using ARIMA**
R allows us to find the appropriate ARIMA model that fits our series through the auto.arima() function which determines all 3 parameters that best express our seris.

Let's try it on our data. We will use the adj_days series which is the series adjusted for number of days as we saw that it has less variance that the original time series

```r
season_arima<-auto.arima(adj_days)
summary(season_arima)
```

```
## Series: adj_days 
## ARIMA(1,1,1)(0,1,1)[12] 
## 
## Coefficients:
##           ar1      ma1     sma1
##       -0.2122  -0.6782  -0.5475
## s.e.   0.1236   0.0970   0.0988
## 
## sigma^2 estimated as 18855:  log likelihood=-679.59
## AIC=1367.19   AICc=1367.58   BIC=1377.88
## 
## Training set error measures:
##                  ME     RMSE      MAE         MPE     MAPE      MASE
## Training set 5.9553 127.8321 93.70135 0.001487367 3.123973 0.4927328
##                      ACF1
## Training set -0.009056928
```

The function determines that the optimal model for our time series is ARIMA(1,1,1)(0,1,1)12 this means
using auto correlation form lag-1
first order non-seasonal differencing
using errors with lag-1
then from a seasonality with a frequency of 12(unsurprinsigly given our data)
use a first order seasonal differencing
use lag errors from one season before

Let's apply this to get 2019 data


```r
season_arima%>%forecast(h=12)%>%autoplot()+ggtitle("Arima 2019")
```

![](Tourism_time_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

Let's compare our ARIMA predictions with the predictions from our regression.
To be fair in our comparaison, we should also run our ARIMA on data as from 2012 (to account for change in October trends as explained above).

```r
season_arima_model<-auto.arima(window(adj_days,start=2012))
season_arima_2019<-season_arima_model%>%forecast(h=12)

autoplot(season_arima_2019)+ggtitle("Arima 2019 from 2012")
```

![](Tourism_time_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

Let's compare our regression forecast with the ARIMA forecast and the total with the official 2019 figures

```r
tou_update<-tou%>%mutate("2019"=c(122273,115613,114419,108565,96814,92398,115448,107275,100837,129018,128730,152098)) #adding official 2019

tou_update<-tou_update%>% subset(month!="Year")
tou_ts_2019<-ts(as.numeric(as.matrix(tou_update))[13:144],frequency = 12,start=2009)
```

```
## Warning in is.data.frame(data): NAs introduced by coercion
```

```r
adj_days_2019<-tou_ts_2019/monthdays(tou_ts_2019)

autoplot(adj_days_2019)+autolayer(arri_model_2019,PI=FALSE,series = "Regression")+autolayer(season_arima_2019,PI=FALSE,series="ARIMA")
```

![](Tourism_time_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

The two models look tremendously close, nearly indistinguishable. But both seem to understate their figures compared to the official figures for nearly the whole period

Note that the differences are average arrival of tourists per day so scaled up to months, the differences might be slightly more noticeable.

Comparing where the differences between the forecasts and the actual model, for that we will use the RMSE


```r
compara_2019<-as_tibble(cbind(adj_days_2019,arri_model_2019$mean,season_arima_2019$mean))%>%subset(`arri_model_2019$mean`!="NA")

rmse(compara_2019$adj_days_2019,compara_2019$`arri_model_2019$mean`)#actual vs regression
```

```
## [1] 313.9106
```

```r
rmse(compara_2019$adj_days_2019,compara_2019$`season_arima_2019$mean`)#actual vs ARIMA
```

```
## [1] 304.5561
```
The ARIMA model seem to have a lower RMSE, but still missing the mark by an average of 304 arrivals per day is quite the undeperformance, To put it in perspective that would be equivalent to approxiamtely 10% error rate (mean of daily arrival for 2019=3064 toursits per day)

However the question is, how much is it the model underperforming or much, maybe comes from a change in the data for 2019 itself?

Let's check that by seeing the plot alone


```r
autoplot(adj_days_2019)
```

![](Tourism_time_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
From the above plot, it seems that there actually is a decline from 2018 to 2019 in the arrival of tourists. It seemed that the decreasing trend in arrivals went the opposite direction after the first few months of 2019.

let's check the magnitude of this change by creating a function



```r
######start here and link previous code to this one######
arriv_change<-function(df,col1,col2){
  (mean(df[[col2]])-mean(df[[col1]]))/mean(df[[col1]])
}# creating function

arriv_change(tou_update,"2018 1","2019")#2018 1 because that is how the column is named in our data
```

```
## [1] -0.01137624
```

```r
arriv_change(tou_update,"2017","2018 1")
```

```
## [1] 0.04288674
```

```r
arriv_change(tou_update,"2016","2017")
```

```
## [1] 0.05225187
```

```r
arriv_change(tou_update,"2015","2016")
```

```
## [1] 0.1076871
```

```r
arriv_change(tou_update,"2014","2015")
```

```
## [1] 0.1087492
```

```r
arriv_change(tou_update,"2013","2014")
```

```
## [1] 0.04617719
```

```r
arriv_change(tou_update,"2012","2013")
```

```
## [1] 0.02803071
```

From our function it would seem the number of arrivals had an increasing growth up till 2014-2015 then it started having a decreasing growth until 2019 where tourist arrivals decreased. This "sharper" decrease which happens quite early in 2019( as seen in our plot) is probably not related to the covid-19, at least from my (non-specialist fo the tourism industry) point of view.
 However it helps explain the relatively high level of variation between our models and the actual figures.
 
All that being said, the ARIMA model did seem to pick up the decreasing trend better than the time-series regression( as seen by the lower RMSE), consequently for our task of predicting the impact of closed frontieres on the tourist arrivals, we will be using the ARIMA model, while retraining it with the official 2019 data.
**************************************************************************
**Forecasting 2020**
Now the hard part, estimating 2020 data. 
For the context, Mauritius has had its borders closed in 2020 for May,June and now July.

There is no perfect way of predicting how the rest of 2020 will be affected. One key reason for that is because even if we were to take the arrrival levels up to now (with May,June and July at zero) in an ex-post ante fashion, it wouldnt account for other consequences of the pandemic like changes in mood around holiday, financial crisis and general risk aversion.

Usually in such a situatio the model should be combined with an industry expert so that predictions can be readjusted.

For starters,lets start with just running our model on the 3 months of closed frontiers and see how it affects our predictions.This will assume that after those months, the normal pattern of tourism arrival will continue(thus the same autocorrelations and MA as before the closure will be respected)

Since the numbers for January to April 2020 are not reported yet, I wil assume they are the same as that of 2019(a seasonal naive), then we will account for the months with closed frontier and predict the rest of the year.



```r
tou_update_current<-tou_update%>%add_column("2020"=c(122273,115613,114419,8565,0,0,0,0,0,0,0,0))
tou_ts_current<-ts(as.numeric(as.matrix(tou_update_current))[13:156],frequency = 12,start=2009)
```

```
## Warning in is.data.frame(data): NAs introduced by coercion
```

```r
adj_current<-round(tou_ts_current/monthdays(tou_ts_current),0)#adjust for month days

autoplot(adj_current)
```

![](Tourism_time_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Our Arima model will capture hav edifferent ARIMA parameters as our 2019 model because the autocorrelations and Moving averages of error, both seasonally and non-seasonally will be affected by the new figures.


```r
current_arima_model<-auto.arima(window(adj_current,start=2012,end=c(2020,7)))
summary(current_arima_model)
```

```
## Series: window(adj_current, start = 2012, end = c(2020, 7)) 
## ARIMA(1,1,0)(1,1,0)[12] 
## 
## Coefficients:
##           ar1     sar1
##       -0.2006  -0.3782
## s.e.   0.1058   0.1633
## 
## sigma^2 estimated as 159233:  log likelihood=-666.65
## AIC=1339.31   AICc=1339.59   BIC=1346.81
## 
## Training set error measures:
##                     ME     RMSE     MAE MPE MAPE      MASE        ACF1
## Training set -39.54348 368.8409 162.671 NaN  Inf 0.4805251 -0.01441304
```
Ok now our model is ARIMA(1,1,0)(1,1,0)12

That is one autocorrelation and one order of differencing and the same thing seasonally.

I wil try to run a regression model to see what differences there might be

Let's predict


```r
season_arima_current<-current_arima_model%>%forecast(h=5)

arri_model_current<-tslm(window(adj_current,start=2012,end=c(2020,7))~trend+season)
arri_reg_current<-forecast(arri_model_current,h=5)


autoplot(adj_current)+autolayer(season_arima_current,PI=FALSE,series="Arima with closed borders")+autolayer(arri_reg_current,PI=FALSE,series="Regression with closed borders")
```

![](Tourism_time_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

Well....damn.

We have two wildly varying predictions and I would say both are wrong.

The regression is clearly not very affected by the change in trend after 2019, it just resumes things as normal with a small decrease.
We might say it downplays the effect of the 2020 period.

The arrima does the opposite, since it relies on AR and MA, both are quite affected by the change in late 2019 and especially 2020, this results in a model that maps our time series with autocorrelations and moving averages that include the effects of 2020. This isnt really true because most likely (in my non-tourism specialist opinion) tourist activity will respect the same patterns after covid(albeit with different magnitude). Thus our ARIMA overplays the effect of 2020.

Note that the negative tourists can be for our exercise's purpose be substituted for zero.

**Expert**

In such times of unprecendented events, the right way to proceed is to look for advice from an industry specialist.

The latter can either shed light on how dependant variables change or/and help understand the effects of other unaccounted for variables on the independant one.

In this situation we will use the OECD as our expert (https://www.oecd.org/coronavirus/policy-responses/tourism-policy-responses-to-the-coronavirus-covid-19-6466aa20/#:~:text=The%20coronavirus%20(COVID-19)%20pandemic%20has%20triggered%20an%20unprecedented,recovery%20is%20delayed%20until%20December.)

They expect a 60% decrease in international tourism and in case of prolonged delays 80%.

So to incorporate that in our model we will run our model on our data including the official 2019 figures. And this time we will predict year 2020 under normal conditions and then adjust the results by 80%.

For this we will just go with the ARIMA which was the superior model in normal situation (as evaluated by rmse before)


```r
arima_model_2020<-auto.arima(window(adj_days_2019,start=2012))
summary(arima_model_2020)
```

```
## Series: window(adj_days_2019, start = 2012) 
## ARIMA(1,1,1)(0,1,1)[12] 
## 
## Coefficients:
##           ar1      ma1     sma1
##       -0.2362  -0.6551  -0.3832
## s.e.   0.1413   0.1154   0.1205
## 
## sigma^2 estimated as 24170:  log likelihood=-536.5
## AIC=1081.01   AICc=1081.52   BIC=1090.69
## 
## Training set error measures:
##                    ME     RMSE      MAE        MPE     MAPE      MASE
## Training set 1.329024 141.9228 103.6937 0.07935575 3.107608 0.5004296
##                    ACF1
## Training set 0.02583934
```

Note how the model arima_2020 is similar to arima_2019. Both seasonally and normally the same ARIMA model fit our series even when we add the 2019 information.

Now forecast


```r
season_arima_2020<-arima_model_2020%>%forecast(h=12)
autoplot(season_arima_2020,PI=FALSE)
```

![](Tourism_time_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

The model's prediction for 2020 are interesting, they predict lower daily average arrivals than 2019 at the lowest point and higher at the highest point.

Let's see how this looks out now when we take the OECD advice and apply the 80% slash.




```r
autoplot(adj_days_2019)+autolayer(season_arima_2020$mean*0.2)
```

![](Tourism_time_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

That is quite ugly:S.

If the OECD's expectations are correct and the model made reasonable predictions, we would be facing arrivals level below 1000 daily average arrivals. 

But it is even more dire if we are to take into account that April-July and potentially August are months with zero arrival, though the fact that January- March had "normal" levels, can be a bit reasurring.

For simplification purposes we will assume that these first 3 full months combine witht he now 4/5 empty month balance for a 80% reduction over the period.

Let's adjust these expected arrivals to a monthly level and add it to our dataframe

```r
month_2020<-round((season_arima_2020$mean*0.2)*monthdays(adj_current),0)

tou_update_2020<-mutate(tou_update,"2020"=month_2020)

year_total<-colSums(tou_update_2020[,2:13]) #getting total number of tourist for each year

plot(year_total)
```

![](Tourism_time_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

*Putting a monetary value*

(Note: this has nothing to do with time-series anymore)

Thats all good but what does this decrease of tourist arrival mean to the island?

Actually how much does a tourist bring to Mauritius?

In 2019 the total tourist earnings was estimated to be approx Rs.63,107,000,000 (63.1 billion approx-where Rs is Mauritian Rupees).

An accurate breakdown of this amount is not available, but we can make an approximation for the economic "value" of one tourist.


```r
tourist_value<-round((63.1/year_total[11])*10^9,0) #note that year_total[11] is for 2019
tourist_value
```

```
##  2019 
## 45609
```

So one tourist brings approximately Rs.45609.

Note that this number is an average and should be interpreted as such. The distribution can be very varried. Given's Mauritius positioning as both a high end destination for Europe and a day-shopping place for surrounding countries, we could estimate the distribution being u-shaped(that is my personal imagination of distribution).

It should also note that this 

So now with this value we can estimate the value from tourism that we will obtain in 2020. And we will also see the decrease in 


```r
year_total[12]*tourist_value
```

```
##        2020 
## 12625939470
```

So as per our forecast tourism in 2020 will bring 5* less money than what it did in 2019 on average.

With this being said, I think we can only hope that our predictions are wrong...





















