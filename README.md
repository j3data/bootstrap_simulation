How does stock market volatility impact returns?
================



###### A bootstrap simulation is used to evaluate the performance of three portfolios (growth, balanced, and conservative) during three different volatility periods (average, high, and low).

ETL
---

The source data is composed of daily returns by symbol. Dates are not aligned in the data and will be aligned in the code.

Check Data
----------

The loaded daily returns are compared to known year-end figures.

We will verify
- Missing data is not significant.
- Daily returns are correct.
- The annual return calculation is correct.

#### Check for missing data

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

#### Results

-   TLT: No missing data.
-   SPY: No missing data.
-   USGG1M (Risk Free Rate): Missing value on '2015-05-08'.

#### Verify Returns

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

#### Results

-   The calculated annual returns match the actual annual returns from Morningstar for 2015.
-   <http://performance.morningstar.com/funds/etf/total-returns.action?t=SPY>
-   <http://performance.morningstar.com/funds/etf/total-returns.action?t=TLT>

Define Time Periods
-------------------

Create time periods for average, high, and low, volatility periods.

##### Average volatility period

This 10 year period includes daily returns from 04/01/2007 to 03/31/2017.

    ## [1] 2518

⇒ There are 2,518 days in average volatility time period.

##### High volatility period

This time period includes days where the CBOE Volatility Index "VIX" is greater than 30. Days in this time period range from 9/15/08 to 5/18/09. This time period coincides with the financial crisis.

    ## [1] 170

⇒ There are 170 days in high volatility time period.

##### Low volatility period

The low volatility time period includes days where the VIX is always less than 20 and often less than 15. This time period includes dates from 11/01/04 to 1/1/07 AND from 1/4/2013 to 10/3/14.

    ## [1] 987

⇒ There are 987 days in low volatility time period.

Simulation I: Normally Distributed Returns
------------------------------------------

The first simulation attempt uses values randomly generated from the normal distribution. The normal distribution is fit with the mean and standard deviation observed from the sample data.

``` r
SPY_mean <- mean(SPY_daily_returns)
SPY_sd   <- sd(SPY_daily_returns)
SPY_sim_returns <- rnorm(avg_vol_day_count,SPY_mean,SPY_sd)
```

#### Do simulated returns and observed returns come from the same distribution?

Two-sample Kolmogorov-Smirnov Test

``` r
ks.test(SPY_daily_returns, SPY_sim_returns)
```

    ## Warning in ks.test(SPY_daily_returns, SPY_sim_returns): p-value will be
    ## approximate in the presence of ties

    ## 
    ##  Two-sample Kolmogorov-Smirnov test
    ## 
    ## data:  SPY_daily_returns and SPY_sim_returns
    ## D = 0.1112, p-value = 6.006e-14
    ## alternative hypothesis: two-sided

![](simulation_summary_files/figure-markdown_github/unnamed-chunk-10-1.png)

There is significant evidence (p-value &lt; .0001) that the normally simulated SPY returns and actually observed SPY returns are not from the same distribution. In other words, the observed returns are not normally distributed. The normal simulation is a poor method of generating stock returns.

Simulation II: Bootstrap Method
-------------------------------

Stock returns are simulated by sampling with replacement from the three chosen time periods. The three time periods include average volatility, high volatility, and low volatility. Each loop of the simulation generates 10 years of daily returns and sharpe ratios.

The Sharpe ratio represents a measure of the portfolio's risk-adjusted return.

Sharpe Ratio = Average(Excess Return) / sd(Excess Return)
Excess Return = Daily Return - Risk Free Rate

$$\\frac{a+b}{b}$$
$$ Sharpe Ratio = \\frac{\\overline{ExcessReturn}}{ExcessReturn}$$
 Sharpe Ratio = Excess Return sd(excess return)
*E**x**c**e**s**s**R**e**t**u**r**n* = *D**a**i**l**y**R**e**t**u**r**n* − *R**i**s**k**F**r**e**e**R**a**t**e*
*H*<sub>0</sub> : *β*<sub>1</sub> = 0

#### Bootstrap Parameters

``` r
loops <- 100                # Number of simulations loops
days  <- avg_vol_day_count  # Days in 10 year time period
```

#### Bootstrap Function

``` r
ret.fun = function(x){  
  one_sample <- data.table(Date=sample(x, days, replace=T), key="Date")
  result <- merge(one_sample,market_data,all.x = TRUE)
  result$growth_Return_Index        <- (result$growth_return       / 100) + 1
  result$balanced_Return_Index      <- (result$balanced_return     / 100) + 1
  result$conservative_Return_Index  <- (result$conservative_return / 100) + 1
  
  annual_return_growth       <- (prod(result$growth_Return_Index      , na.rm = FALSE) - 1)*100
  annual_return_balanced     <- (prod(result$balanced_Return_Index    , na.rm = FALSE) - 1)*100
  annual_return_conservative <- (prod(result$conservative_Return_Index, na.rm = FALSE) - 1)*100
  
  sharpe_ratio_growth        <- mean(result$growth_excess_return)   /sd(result$growth_excess_return      )
  sharpe_ratio_balanced      <- mean(result$balanced_excess_return) /sd(result$balanced_excess_return    )
  sharpe_ratio_conservative  <- mean(result$conservative_return)    /sd(result$conservative_excess_return)

  return(Map(cbind, annual_return_growth, annual_return_balanced, annual_return_conservative, 
             sharpe_ratio_growth, sharpe_ratio_balanced, sharpe_ratio_conservative))  
}
```

#### Average Volatility Simulation: Returns and Sharpe Ratios

``` r
avg_sim <- replicate(loops, ret.fun(avg_vol_days), simplify = "array")
avg_vol_sim <-  data.table(t(matrix(unlist(avg_sim), nrow=length(unlist(avg_sim[1])))))
setnames(avg_vol_sim,simulation_columns)

print(avg_vol_sim[1:5])              #Table Excerpt
```

    ##    annual_return_growth annual_return_balanced annual_return_conservative
    ## 1:            245.79206              223.22204                   176.1394
    ## 2:             72.79752               94.30175                   124.9662
    ## 3:            131.24485              131.56872                   120.6032
    ## 4:            -17.19196               26.37886                   143.7071
    ## 5:            110.93675              138.33974                   178.2077
    ##    sharpe_ratio_growth sharpe_ratio_balanced sharpe_ratio_conservative
    ## 1:         0.054219056            0.06348090                0.07399440
    ## 2:         0.025234881            0.03580773                0.05789954
    ## 3:         0.036401080            0.04467516                0.05903426
    ## 4:        -0.003650078            0.01402999                0.06281215
    ## 5:         0.034047978            0.04805979                0.07482013

#### High Volatility Simulation: Returns and Sharpe Ratios

``` r
hi_sim <- replicate(loops, ret.fun(high_vol_days), simplify = "array")
high_vol_sim  <-  data.table(t(matrix(unlist(hi_sim), nrow=length(unlist(hi_sim[1])))))
setnames(high_vol_sim,simulation_columns)

print(high_vol_sim[1:5])              #Table Excerpt
```

    ##    annual_return_growth annual_return_balanced annual_return_conservative
    ## 1:            -98.69845              -96.78058                  -88.24173
    ## 2:            -98.22344              -95.33646                  -81.13472
    ## 3:            -99.29428              -97.61540                  -85.43251
    ## 4:            -99.63989              -98.23662                  -80.20415
    ## 5:            -80.86667              -75.85906                  -71.40899
    ##    sharpe_ratio_growth sharpe_ratio_balanced sharpe_ratio_conservative
    ## 1:         -0.05681575           -0.06059995               -0.06543413
    ## 2:         -0.05052595           -0.05132073               -0.04745734
    ## 3:         -0.06456785           -0.06480569               -0.05712623
    ## 4:         -0.07436672           -0.07046865               -0.04695094
    ## 5:         -0.01276957           -0.01806777               -0.03403270

#### Low Volatility Simulation: Returns and Sharpe Ratios

``` r
low_sim <- replicate(loops, ret.fun(low_vol_days), simplify = "array")
low_vol_sim  <-  data.table(t(matrix(unlist(low_sim), nrow=length(unlist(low_sim[1])))))
setnames(low_vol_sim,simulation_columns)

print(low_vol_sim[1:5])              #Table Excerpt
```

    ##    annual_return_growth annual_return_balanced annual_return_conservative
    ## 1:             166.5421               139.4883                   98.01959
    ## 2:             377.6275               278.5080                  153.76792
    ## 3:             326.6328               301.7932                  259.37518
    ## 4:             261.8650               197.2238                  111.50748
    ## 5:             161.1643               132.5927                   89.60713
    ##    sharpe_ratio_growth sharpe_ratio_balanced sharpe_ratio_conservative
    ## 1:          0.06787607            0.06775703                0.06543162
    ## 2:          0.11124970            0.10658217                0.08619915
    ## 3:          0.10091416            0.10980972                0.11790200
    ## 4:          0.09233991            0.08813344                0.07172205
    ## 5:          0.06684982            0.06519259                0.06067423

Bootstrap Output Overview
-------------------------

#### Annual Return Plots

![](simulation_summary_files/figure-markdown_github/unnamed-chunk-18-1.png)

#### Annual Return Summary Statistics

<table style="width:71%;">
<colgroup>
<col width="30%" />
<col width="12%" />
<col width="27%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Portfolio Group</th>
<th align="center">Median</th>
<th align="center">Standard Deviation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Low Vol Growth</td>
<td align="center">251.3</td>
<td align="center">100.8</td>
</tr>
<tr class="even">
<td align="center">Avg Vol Growth</td>
<td align="center">130.9</td>
<td align="center">119</td>
</tr>
<tr class="odd">
<td align="center">High Vol Growth</td>
<td align="center">-95.9</td>
<td align="center">24.3</td>
</tr>
<tr class="even">
<td align="center">Low Vol Balanced</td>
<td align="center">198.8</td>
<td align="center">75.4</td>
</tr>
<tr class="odd">
<td align="center">Avg Vol Balanced</td>
<td align="center">130.9</td>
<td align="center">90.2</td>
</tr>
<tr class="even">
<td align="center">High Vol Balanced</td>
<td align="center">-89.7</td>
<td align="center">31.5</td>
</tr>
<tr class="odd">
<td align="center">Low Vol Conservative</td>
<td align="center">126.3</td>
<td align="center">57.6</td>
</tr>
<tr class="even">
<td align="center">Avg Vol Conservative</td>
<td align="center">117.1</td>
<td align="center">67.9</td>
</tr>
<tr class="odd">
<td align="center">High Vol Conservative</td>
<td align="center">-59.1</td>
<td align="center">44.3</td>
</tr>
</tbody>
</table>

#### Sharpe Ratio Plots

![](simulation_summary_files/figure-markdown_github/unnamed-chunk-22-1.png)

#### Sharpe Ratio Summary Statistics

<table style="width:71%;">
<colgroup>
<col width="30%" />
<col width="12%" />
<col width="27%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Portfolio Group</th>
<th align="center">Median</th>
<th align="center">Standard Deviation</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Low Vol Growth</td>
<td align="center">0.089</td>
<td align="center">0.022</td>
</tr>
<tr class="even">
<td align="center">Avg Vol Growth</td>
<td align="center">0.036</td>
<td align="center">0.02</td>
</tr>
<tr class="odd">
<td align="center">High Vol Growth</td>
<td align="center">-0.037</td>
<td align="center">0.023</td>
</tr>
<tr class="even">
<td align="center">Low Vol Balanced</td>
<td align="center">0.087</td>
<td align="center">0.022</td>
</tr>
<tr class="odd">
<td align="center">Avg Vol Balanced</td>
<td align="center">0.046</td>
<td align="center">0.02</td>
</tr>
<tr class="even">
<td align="center">High Vol Balanced</td>
<td align="center">-0.035</td>
<td align="center">0.024</td>
</tr>
<tr class="odd">
<td align="center">Low Vol Conservative</td>
<td align="center">0.077</td>
<td align="center">0.022</td>
</tr>
<tr class="even">
<td align="center">Avg Vol Conservative</td>
<td align="center">0.056</td>
<td align="center">0.02</td>
</tr>
<tr class="odd">
<td align="center">High Vol Conservative</td>
<td align="center">-0.023</td>
<td align="center">0.025</td>
</tr>
</tbody>
</table>
