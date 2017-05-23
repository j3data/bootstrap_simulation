How does stock market volatility impact returns?
================

A bootstrap simulation is used to evaluate the performance of three portfolios (growth, balanced, and conservative) during three different volatility periods (average, high, and low).

ETL
---

The source data is composed of daily returns by symbol. Dates are not aligned in the data and will aligned in the code.

Check Data
----------

The loaded daily returns are compared to known year-end figures.

We will verify:
(1) Missing data is not significant.
(2) Daily returns are correct.
(3) The annual return calculation is correct.

### Check for missing data

``` r
start_date <- '2004-11-01'
stop_date  <- '2016-12-31'

market_data[Date >= start_date & Date <= stop_date & is.na(TLT_Return_Pct),]
```

``` r
market_data[Date >= start_date & Date <= stop_date & is.na(SPY_Return_Pct),]
```

``` r
market_data[Date >= start_date & Date <= stop_date & is.na(USGG1M_Return_Pct),]
```

### Results:

TLT: No missing data.
SPY: No missing data.
USGG1M (Risk Free Rate): Missing rate on '2015-05-08'.

### Verify Returns

``` r
start_date <- '2015-01-01'
stop_date  <- '2015-12-31'
market_data_cut <- market_data[Date >= start_date & Date <= stop_date, ]

market_data_cut$SPY_Return_Index <- (market_data_cut$SPY_Return_Pct / 100) + 1
spy_annual_return <- ( prod(market_data_cut$SPY_Return_Index, na.rm = FALSE) - 1)*100
spy_annual_return
```

    ## [1] 1.252234

``` r
market_data_cut$TLT_Return_Index <- (market_data_cut$TLT_Return_Pct / 100) + 1
tlt_annual_return <- ( prod(market_data_cut$TLT_Return_Index, na.rm = FALSE) - 1)*100
tlt_annual_return
```

    ## [1] -1.787336

Annual returns match expected returns from Morningstar
<http://performance.morningstar.com/funds/etf/total-returns.action?t=SPY>
<http://performance.morningstar.com/funds/etf/total-returns.action?t=TLT>

Define Time Periods
-------------------

Create time periods for average, high, and low, volatility periods.

##### 1. Average volatility period

This 10 year period includes daily returns from 04/01/2007 to 03/31/2017

    ## [1] 2518

There are 2,518 days in average volatility time period

##### 2. High volatility period

This time period includes days where the CBOE Volatility Index "VIX" is greater than 30.
Days in this time period range from 9/15/08 to 5/18/09.
This time period coincides with the financial crisis.

    ## [1] 170

There are 170 days in high volatility time period.

##### 3. Low volatility period

The low volatility time period includes days where the VIX is always less than 20 and often less than 15.
This time period includes dates from 11/01/04 to 1/1/07 AND from 1/4/2013 to 10/3/14.

    ## [1] 987

There are 987 days in low volatility time period.

Simulation: Normally Distributed Returns
----------------------------------------

The first simulation attempt will use values randomly generated from the normal distribution.
The normal distribution will be fit with the mean and standard deviation observed from the sample data.

#### Do simulated returns and observed returns come from the same distribution?

Two-sample Kolmogorov-Smirnov test

``` r
ks.test(SPY_daily_returns, SPY_sim_returns)
```

    ## Warning in ks.test(SPY_daily_returns, SPY_sim_returns): p-value will be
    ## approximate in the presence of ties

    ## 
    ##  Two-sample Kolmogorov-Smirnov test
    ## 
    ## data:  SPY_daily_returns and SPY_sim_returns
    ## D = 0.10207, p-value = 8.113e-12
    ## alternative hypothesis: two-sided

![](simulation_summary_files/figure-markdown_github/unnamed-chunk-10-1.png)

There is significant evidence (p-value &lt; .0001) that the normally simulated SPY returns and actually observed SPY returns are not from the same distribution.
In other words, the observed returns are not normally distributed.
The normal simulation is not appropriate. Another method must be used.

Simulation: Bootstrap Method
----------------------------

A simulation is run where data is sampled with replacement from three time periods.
The three time periods include average volatility, high volatility, and low volatility.
The simulation generates 10 year returns and sharpe ratios.

The Sharpe Ratio measures risk adjusted returns.
Sharpe ratio = average(excess return) / sd(excess return)
Excess return = daily return - risk free rate

##### Bootstrap Parameters

``` r
loops <- 100                # Number of simulations loops
days  <- avg_vol_day_count  # Days in 10 year time period
```

##### Function

``` r
ret.fun = function(x){  
  one_sample <- data.table(Date=sample(x, days, replace=T), key="Date")
  result <- merge(one_sample,market_data,all.x = TRUE)
  result$growth_Return_Index        <- (result$growth_return / 100) + 1
  result$balanced_Return_Index      <- (result$balanced_return / 100) + 1
  result$conservative_Return_Index  <- (result$conservative_return / 100) + 1
  
  annual_return_growth       <- (prod(result$growth_Return_Index, na.rm = FALSE) - 1)*100
  annual_return_balanced     <- (prod(result$balanced_Return_Index, na.rm = FALSE) - 1)*100
  annual_return_conservative <- (prod(result$conservative_Return_Index, na.rm = FALSE) - 1)*100
  
  sharpe_ratio_growth       <- (mean(result$growth_excess_return)  /sd(result$growth_excess_return      ))
  sharpe_ratio_balanced     <- (mean(result$balanced_excess_return)/sd(result$balanced_excess_return    ))
  sharpe_ratio_conservative <- (mean(result$conservative_return)   /sd(result$conservative_excess_return))

  return(Map(cbind, annual_return_growth, annual_return_balanced, annual_return_conservative, 
             sharpe_ratio_growth, sharpe_ratio_balanced, sharpe_ratio_conservative))  
}
```

##### Average Volatility Simulation: Returns and Sharpe Ratios

``` r
avg_sim <- replicate(loops, ret.fun(avg_vol_days), simplify = "array")
avg_vol_sim <-  data.table(t(matrix(unlist(avg_sim), nrow=length(unlist(avg_sim[1])))))
setnames(avg_vol_sim,c('V1','V2','V3','V4', 'V5', 'V6'), 
         c('annual_return_growth', 'annual_return_balanced', 'annual_return_conservative',
           'sharpe_ratio_growth', 'sharpe_ratio_balanced', 'sharpe_ratio_conservative'))
head(avg_vol_sim)
```

    ##    annual_return_growth annual_return_balanced annual_return_conservative
    ## 1:            133.64408              113.23903                   75.05703
    ## 2:            619.10752              424.81851                  196.38175
    ## 3:             94.46352              125.63736                  175.15935
    ## 4:             65.06871               60.72554                   46.62644
    ## 5:            143.69695              164.26819                  188.41573
    ## 6:            150.55674              195.48255                  271.65938
    ##    sharpe_ratio_growth sharpe_ratio_balanced sharpe_ratio_conservative
    ## 1:          0.03853830            0.04190739                0.04108550
    ## 2:          0.08466989            0.08903254                0.07628130
    ## 3:          0.03013749            0.04392693                0.07267707
    ## 4:          0.02407796            0.02714552                0.02970800
    ## 5:          0.03940181            0.05193066                0.07381527
    ## 6:          0.04123123            0.05876494                0.09194513

##### High Volatility Simulation: Returns and Sharpe Ratios

``` r
hi_sim <- replicate(loops, ret.fun(high_vol_days), simplify = "array")
high_vol_sim  <-  data.table(t(matrix(unlist(hi_sim), nrow=length(unlist(hi_sim[1])))))
setnames(high_vol_sim,c('V1','V2','V3','V4', 'V5', 'V6'), 
         c('annual_return_growth', 'annual_return_balanced', 'annual_return_conservative',
           'sharpe_ratio_growth', 'sharpe_ratio_balanced', 'sharpe_ratio_conservative'))
head(high_vol_sim)
```

    ##    annual_return_growth annual_return_balanced annual_return_conservative
    ## 1:            -97.59065             -93.618588                  -73.96261
    ## 2:            -98.65227             -95.490898                  -72.87480
    ## 3:            -99.26277             -97.790875                  -89.00417
    ## 4:            -99.23814             -96.605762                  -66.99047
    ## 5:            -97.57975             -94.099586                  -79.19441
    ## 6:            -38.24184               5.057065                  103.29589
    ##    sharpe_ratio_growth sharpe_ratio_balanced sharpe_ratio_conservative
    ## 1:        -0.045491102           -0.04507127               -0.03770540
    ## 2:        -0.054879998           -0.05235492               -0.03652334
    ## 3:        -0.064586384           -0.06722534               -0.06693614
    ## 4:        -0.064842288           -0.05899640               -0.03064492
    ## 5:        -0.044323183           -0.04560834               -0.04397820
    ## 6:         0.005576661            0.01101022                0.02850361

##### Low Volatility Simulation: Returns and Sharpe Ratios

``` r
low_sim <- replicate(loops, ret.fun(low_vol_days), simplify = "array")
low_vol_sim  <-  data.table(t(matrix(unlist(low_sim), nrow=length(unlist(low_sim[1])))))
setnames(low_vol_sim,c('V1','V2','V3','V4', 'V5', 'V6'), 
         c('annual_return_growth', 'annual_return_balanced', 'annual_return_conservative',
           'sharpe_ratio_growth', 'sharpe_ratio_balanced', 'sharpe_ratio_conservative'))
head(low_vol_sim)
```

    ##    annual_return_growth annual_return_balanced annual_return_conservative
    ## 1:             316.5126               270.1016                  200.44127
    ## 2:             230.6915               172.2585                   94.62514
    ## 3:             263.2874               210.3658                  136.00935
    ## 4:             275.5774               231.0605                  164.93412
    ## 5:             126.7950               105.8509                   73.10363
    ## 6:             321.9160               302.9826                  269.02720
    ##    sharpe_ratio_growth sharpe_ratio_balanced sharpe_ratio_conservative
    ## 1:          0.10023545            0.10461064                0.10395834
    ## 2:          0.08521939            0.07924983                0.06291281
    ## 3:          0.09367694            0.09267508                0.08275525
    ## 4:          0.09120810            0.09319092                0.08881352
    ## 5:          0.05551514            0.05427113                0.05277762
    ## 6:          0.10445031            0.11497192                0.12319960

Plotting Output: Returns
------------------------

![](simulation_summary_files/figure-markdown_github/unnamed-chunk-17-1.png)

Plotting Output: Sharpe Ratios
------------------------------

![](simulation_summary_files/figure-markdown_github/unnamed-chunk-19-1.png)

Summary Statistics: Returns
---------------------------

``` r
returns_df
```

    ##         Portfolio Group            Median Standard Deviation
    ## 1        low vol growth  280.807901428282   103.307804670237
    ## 2        avg vol growth  117.543882412936   115.335625763508
    ## 3       high vol growth -96.6629277782454   50.5194019543193
    ## 4      low vol balanced  214.272202377965   80.1570430182307
    ## 5      avg vol balanced   125.32435594453   83.9416931331843
    ## 6     high vol balanced -91.3802662909896   40.6222265980851
    ## 7  low vol conservative  133.740353606846   61.6399712365612
    ## 8  avg vol conservative  124.553706195325   61.4199128436606
    ## 9 high vol conservative -65.1138419946153   34.8005968034093

Summary Statistics: Sharpe Ratios
---------------------------------

``` r
sharpe_df
```

    ##         Portfolio Group              Median Standard Deviation
    ## 1        low vol growth  0.0948342174169251 0.0203687448389724
    ## 2        avg vol growth  0.0348249980192829  0.018237149417492
    ## 3       high vol growth -0.0397212096318273  0.021339934805064
    ## 4      low vol balanced  0.0923127085825874 0.0212006483206378
    ## 5      avg vol balanced  0.0438634119932003 0.0178477535251361
    ## 6     high vol balanced -0.0382961337336673 0.0215706154606541
    ## 7  low vol conservative  0.0805731642204936 0.0220016657823799
    ## 8  avg vol conservative  0.0590704404875299 0.0178524686261452
    ## 9 high vol conservative -0.0273028526362913 0.0221739035751288
