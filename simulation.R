#install.packages("package_name")

##################
###    ETL    ####
##################

# The source data is composed of daily total returns by symbol. 
# However, the dates are not aligned. Date matching will be done in the code.

library(boot)
library(readxl)
library(data.table)
library(ggplot2)

file_name <- 'market_data.xlsx'
file_path <- 'C:\\statistics\\06 statistical programming\\data\\'
data_location <- paste(file_path,file_name, sep='')

marketdata <- read_excel(data_location, sheet=2, skip=1, na="")

DT <- data.table(marketdata)
dim.data.frame(DT)

# Define stock return columns
SPY      <- na.omit(DT[, 1: 3])
USGG1M   <- na.omit(DT[, 4: 6])
TLT      <- na.omit(DT[, 7: 9])
IWM      <- na.omit(DT[,10:12])
EFA      <- na.omit(DT[,13:15])
EEM      <- na.omit(DT[,16:18])
AGG      <- na.omit(DT[,19:21])
BIL      <- na.omit(DT[,22:24])
DJP      <- na.omit(DT[,25:27])
IYR      <- na.omit(DT[,28:30])
HYG      <- na.omit(DT[,31:33])
EMB      <- na.omit(DT[,34:36])
VTSMX    <- na.omit(DT[,37:39])
VGTSX    <- na.omit(DT[,40:42])
VBMFX    <- na.omit(DT[,43:45])
PGBIX    <- na.omit(DT[,46:48])
BGHYDAA  <- na.omit(DT[,49:51])

portfolio_list  <- list(SPY, USGG1M, TLT, IWM, EFA, EEM, AGG, BIL, 
                        DJP, IYR, HYG, EMB, VTSMX, VGTSX, VBMFX, PGBIX)

portfolio_label <- list("SPY", "USGG1M", "TLT", "IWM", "EFA", "EEM", "AGG", "BIL", 
                        "DJP", "IYR", "HYG", "EMB", "VTSMX", "VGTSX", "VBMFX", "PGBIX")

# Rename columns
for (i in seq_along(portfolio_label)){
  portfolio_list[i] <- lapply( portfolio_list[i] , setNames , 
                               c('Date', paste(portfolio_label[i],"_Return_Pct",sep=''),paste(portfolio_label[i],"_Last",sep='')))
}

# Set Date as key
setkeyF = function(x) setkey(x, "Date")
lapply(portfolio_list, setkeyF)

# Merge on Date
mergeF = function(x,y) merge(x,y, all = TRUE, by = "Date") 
bigtable <- Reduce(mergeF, portfolio_list)
bigtable[, Date := as.IDate(Date)]

# Cleaned data table
market_data <- data.table(bigtable)
setkey(market_data,"Date")                                                                # Set Date as key

# Calculate daily risk free rate
market_data$risk_free_daily_rate <- ((1+(market_data$USGG1M_Last/100))^(1/365)-1)*100     # Interest for government bonds is calculated using a 365-day year

# Calculate excess return
market_data$spy_excess_return <- (market_data$SPY_Return_Pct - market_data$risk_free_daily_rate)
market_data$tlt_excess_return <- (market_data$TLT_Return_Pct - market_data$risk_free_daily_rate)

# Create Portolfio Return
market_data$growth_return       <- (.80*market_data$SPY_Return_Pct + .20*market_data$TLT_Return_Pct)
market_data$balanced_return     <- (.65*market_data$SPY_Return_Pct + .35*market_data$TLT_Return_Pct)
market_data$conservative_return <- (.40*market_data$SPY_Return_Pct + .60*market_data$TLT_Return_Pct)

# Create Portolfio Excess Return
market_data$growth_excess_return       <- (market_data$growth_return       - market_data$risk_free_daily_rate)
market_data$balanced_excess_return     <- (market_data$balanced_return     - market_data$risk_free_daily_rate)
market_data$conservative_excess_return <- (market_data$conservative_return - market_data$risk_free_daily_rate)

# Remove non-trading days
market_data <- market_data[!is.na(SPY_Return_Pct), ]

#########################
##### Check Data  #######
#########################

# The loaded daily returns are compared to known year-end figures.
# We will verify (1) the daily returns and (2) annual return calculation are correct.

# Checking for missing data
start_date <- '2004-11-01'
stop_date  <- '2016-12-31'

market_data[Date >= start_date & Date <= stop_date & is.na(USGG1M_Return_Pct), ] # Missing USGG1M_Return_Pct rate on '2015-05-08'
market_data[Date >= start_date & Date <= stop_date & is.na(SPY_Return_Pct)   , ] # No missing
market_data[Date >= start_date & Date <= stop_date & is.na(TLT_Return_Pct)   , ] # No missing

# Verify annual returns
start_date <- '2015-01-01'
stop_date  <- '2015-12-31'
market_data_cut <- market_data[Date >= start_date & Date <= stop_date, ]

market_data_cut$SPY_Return_Index <- (market_data_cut$SPY_Return_Pct / 100) + 1
annual_return <- ( prod(market_data_cut$SPY_Return_Index, na.rm = FALSE) - 1)*100
annual_return

market_data_cut$TLT_Return_Index <- (market_data_cut$TLT_Return_Pct / 100) + 1
annual_return <- ( prod(market_data_cut$TLT_Return_Index, na.rm = FALSE) - 1)*100
annual_return

# Annual returns match expected returns from Morningstar
# http://performance.morningstar.com/funds/etf/total-returns.action?t=SPY
# http://performance.morningstar.com/funds/etf/total-returns.action?t=TLT

################################
##### Define Time Periods  #####
################################

# Creat time periods to represent high, low, and average volatility regimes.
# Use market_data as source data file.

# (1) Average volatility period (10 years): 04/01/2007 - 03/31/2017

start_date <- '2007-04-01'
stop_date  <- '2017-03-31'
avg_vol_days <- market_data[Date >= start_date & Date <= stop_date & Date != '2015-05-08', Date]
avg_vol_day_count <- length(avg_vol_days)
avg_vol_day_count

# There are 2,518 Days in Average Volatility time period

# (2) High volatility period (VIX >30): 9/15/08 - 5/18/09

start_date <- '2008-09-15'
stop_date  <- '2009-05-18'
high_vol_days <- market_data[Date >= start_date & Date <= stop_date, Date]
high_vol_day_count <- length(high_vol_days)
high_vol_day_count

# There are 170 Days in High Volatility time period

# (3) Low volatility period: 11/01/04 to 1/1/07 AND 1/4/2013 to 10/3/14

start_date1 <- '2004-11-01'
stop_date1  <- '2007-01-01'
start_date2 <- '2013-01-04'
stop_date2  <- '2014-10-03'

range1 <- market_data[Date >= start_date1 & Date <= stop_date1, Date]
range2 <- market_data[Date >= start_date2 & Date <= stop_date2, Date]

low_vol_days <- market_data[Date %in% c(range1,range2),Date] 
low_vol_day_count <- length(low_vol_days)
low_vol_day_count

# There are 987 Days in Low Volatility time period

#################################################
### Simulation: Normally Distributed Returns  ###
#################################################

# Actual returns
SPY_daily_returns <- market_data[Date %in% avg_vol_days,SPY_Return_Pct]

# Simulated values from normal distribution
# Fit with sample mean and sd of observed values
SPY_mean <- mean(SPY_daily_returns)
SPY_sd   <- sd(SPY_daily_returns)
SPY_sim_returns <- rnorm(avg_vol_day_count,SPY_mean,SPY_sd)

# Two-sample Kolmogorov-Smirnov test
# Do simulated returns and observed returns come from the same distribution?
x <- SPY_daily_returns
y <- SPY_sim_returns
ks.test(x, y)
par(mfrow=c(1,1))
plot(ecdf(x), xlim=range(c(x, y)), col = c("blue"), main="Empirical Distribution Function (ECDF)", xlab="Daily Return", ylab="Cumulative Probability")
plot(ecdf(y), add=TRUE, lty=1, col = c("red"))
legend("bottomright",
        legend=c("SPY Actual Returns","SPY Simulated Returns"),
        col = c("blue", "red"), lwd=1, lty=c(1,1 ))

# There is significant evidence that the simulated returns and the observed returns are not from the same distribution.
# In other words, the observed returns are not normally distributed.

##############################
### Bootstrap Simulation  ####
##############################

# A simulation is run where data is sampled with replacement from the three time periods.
# The simulation generates 10 year returns and sharpe ratios.
# Simulations are run for (i) avg_vol_days (ii) high_vol_days and (iii) low_vol_days

# The Sharpe Ratio measures risk adjusted returns.
# Sharpe ratio = average(excess return) / sd(excess return)
# Where excess return = daily return - risk free rate

# Bootstrap parameters
loops <- 10000              # Number of simulations loops
days  <- avg_vol_day_count  # Days in 10 year time period

# Function
ret.fun = function(x){  
  one_sample <- data.table(Date=sample(x, days, replace=T), key="Date")              # Generates sample dates
  result <- merge(one_sample,market_data,all.x = TRUE)                               # Join with market data
  result$growth_Return_Index        <- (result$growth_return / 100) + 1              # Calculate returns
  result$balanced_Return_Index      <- (result$balanced_return / 100) + 1
  result$conservative_Return_Index  <- (result$conservative_return / 100) + 1
  
  annual_return_growth       <- (prod(result$growth_Return_Index, na.rm = FALSE) - 1)*100        # Calculate annulized return
  annual_return_balanced     <- (prod(result$balanced_Return_Index, na.rm = FALSE) - 1)*100
  annual_return_conservative <- (prod(result$conservative_Return_Index, na.rm = FALSE) - 1)*100
  
  sharpe_ratio_growth       <- ( mean(result$growth_excess_return)   / sd(result$growth_excess_return       ))
  sharpe_ratio_balanced     <- ( mean(result$balanced_excess_return) / sd(result$balanced_excess_return     ))
  sharpe_ratio_conservative <- ( mean(result$conservative_return)    / sd(result$conservative_excess_return ))

  return(Map(cbind, annual_return_growth, annual_return_balanced, annual_return_conservative, 
             sharpe_ratio_growth, sharpe_ratio_balanced, sharpe_ratio_conservative))  
}

# (i) avg_vol_days simulation and formatting
avg_sim <- replicate(loops, ret.fun(avg_vol_days), simplify = "array")                      # Simulation of average volatility returns
avg_vol_sim <-  data.table(t(matrix(unlist(avg_sim), nrow=length(unlist(avg_sim[1])))))     # Force output into data table
setnames(avg_vol_sim,c('V1','V2','V3','V4', 'V5', 'V6'), 
         c('annual_return_growth', 'annual_return_balanced', 'annual_return_conservative',
           'sharpe_ratio_growth', 'sharpe_ratio_balanced', 'sharpe_ratio_conservative'))
avg_vol_sim

# (ii) high_vol_days simulation and formatting
hi_sim <- replicate(loops, ret.fun(high_vol_days), simplify = "array")                      # Simulation of high volatility returns
high_vol_sim  <-  data.table(t(matrix(unlist(hi_sim), nrow=length(unlist(hi_sim[1])))))     # Force output into data table
setnames(high_vol_sim,c('V1','V2','V3','V4', 'V5', 'V6'), 
         c('annual_return_growth', 'annual_return_balanced', 'annual_return_conservative',
           'sharpe_ratio_growth', 'sharpe_ratio_balanced', 'sharpe_ratio_conservative'))
high_vol_sim

# (iii) low_vol_days simulation and formatting
low_sim <- replicate(loops, ret.fun(low_vol_days), simplify = "array")                      # Simulation of low volatility returns
low_vol_sim  <-  data.table(t(matrix(unlist(low_sim), nrow=length(unlist(low_sim[1])))))    # Force output into data table
setnames(low_vol_sim,c('V1','V2','V3','V4', 'V5', 'V6'), 
         c('annual_return_growth', 'annual_return_balanced', 'annual_return_conservative',
           'sharpe_ratio_growth', 'sharpe_ratio_balanced', 'sharpe_ratio_conservative'))
low_vol_sim

# Plot Output: Returns
a1 <- low_vol_sim$annual_return_growth
a2 <- avg_vol_sim$annual_return_growth
a3 <- high_vol_sim$annual_return_growth
b1 <- low_vol_sim$annual_return_balanced
b2 <- avg_vol_sim$annual_return_balanced
b3 <- high_vol_sim$annual_return_balanced
c1 <- low_vol_sim$annual_return_conservative
c2 <- avg_vol_sim$annual_return_conservative
c3 <- high_vol_sim$annual_return_conservative
full_range <- c(a1, a2, a3, b1, b2, b3, c1, c2, c3)

# Get y max value for formatting
a1p <- max(hist(a1)$counts,na.rm=TRUE) 
a2p <- max(hist(a2)$counts,na.rm=TRUE) 
a3p <- max(hist(a3)$counts,na.rm=TRUE) 
b1p <- max(hist(b1)$counts,na.rm=TRUE) 
b2p <- max(hist(b2)$counts,na.rm=TRUE) 
b3p <- max(hist(b3)$counts,na.rm=TRUE) 
c1p <- max(hist(c1)$counts,na.rm=TRUE) 
c2p <- max(hist(c2)$counts,na.rm=TRUE) 
c3p <- max(hist(c3)$counts,na.rm=TRUE) 

par(mfrow=c(3,3))

hist(a1,        xlab = 'Growth', main = 'Low Volatility Period', xlim=range(full_range))
abline(v = median(a1), col = "red", lwd = 2)
text(600, a1p*.75 , "Median =", 0)
text(810, a1p*.75 , round(median(a1)), 0)

hist(a2,        xlab = 'Growth',main = 'Average Volatility Period', xlim=range(full_range))
abline(v = median(a2), col = "red", lwd = 2)
text(600, a2p*.75 , "Median =", 0)
text(810, a2p*.75 , round(median(a2)), 0)

hist(a3,       xlab = 'Growth', main = 'High Volatility Period', xlim=range(full_range))
abline(v = median(a3), col = "red", lwd = 2)
text(600, a3p*.75 , "Median =", 0)
text(810, a3p*.75 , round(median(a3)), 0)

hist(b1,      xlab = 'Balanced', main = '',  xlim=range(full_range))
abline(v = median(b1), col = "red", lwd = 2)
text(600, b1p*.75 , "Median =", 0)
text(810, b1p*.75 , round(median(b1)), 0)

hist(b2,  xlab = 'Balanced', main = '', xlim=range(full_range))
abline(v = median(b2), col = "red", lwd = 2)
text(600, b2p*.75 , "Median =", 0)
text(810, b2p*.75 , round(median(b2)), 0)

hist(b3,     xlab = 'Balanced', main = '', xlim=range(full_range))
abline(v = median(b3), col = "red", lwd = 2)
text(600, b3p*.75 , "Median =", 0)
text(810, b3p*.75 , round(median(b3)), 0)

hist(c1,  xlab = 'Conservative', main = '', xlim=range(full_range))
abline(v = median(c1), col = "red", lwd = 2)
text(600, c1p*.75 , "Median =", 0)
text(810, c1p*.75 , round(median(c1)), 0)

hist(c2,      xlab = 'Conservative', main = '', xlim=range(full_range))
abline(v = median(c2), col = "red", lwd = 2)
text(600, c2p*.75 , "Median =", 0)
text(810, c2p*.75 , round(median(c2)), 0)

hist(c3, xlab = 'Conservative', main = '', xlim=range(full_range))
abline(v = median(c3), col = "red", lwd = 2)
text(600, c3p*.75 , "Median =", 0)
text(810, c3p*.75 , round(median(c3)), 0)

##############################
### Plotting Output       ####
##############################

# Plot Output: Sharpe Ratios
x1 <- low_vol_sim$sharpe_ratio_growth
x2 <- avg_vol_sim$sharpe_ratio_growth
x3 <- high_vol_sim$sharpe_ratio_growth
y1 <- low_vol_sim$sharpe_ratio_balanced
y2 <- avg_vol_sim$sharpe_ratio_balanced
y3 <- high_vol_sim$sharpe_ratio_balanced
z1 <- low_vol_sim$sharpe_ratio_conservative
z2 <- avg_vol_sim$sharpe_ratio_conservative
z3 <- high_vol_sim$sharpe_ratio_conservative
full_range_2 <- c(x1, x2, x3, y1, y2, y3, z1, z2, z3)

# Get y max value for formatting
x1p <- max(hist(x1)$counts,na.rm=TRUE) 
x2p <- max(hist(x2)$counts,na.rm=TRUE) 
x3p <- max(hist(x3)$counts,na.rm=TRUE) 
y1p <- max(hist(y1)$counts,na.rm=TRUE) 
y2p <- max(hist(y2)$counts,na.rm=TRUE) 
y3p <- max(hist(y3)$counts,na.rm=TRUE) 
z1p <- max(hist(z1)$counts,na.rm=TRUE) 
z2p <- max(hist(z2)$counts,na.rm=TRUE) 
z3p <- max(hist(z3)$counts,na.rm=TRUE) 

par(mfrow=c(3,3))

hist(x1, xlab = 'Growth', main = 'Low Volatility Period', xlim=range(full_range_2))
abline(v = median(x1), col = "red", lwd = 2)
text(-.10, x1p*.75 , "Median =", 0)
text(-.045, x1p*.75 , round(median(x1),2), 0)

hist(x2, xlab = 'Growth',main = 'Average Volatility Period', xlim=range(full_range_2))
abline(v = median(x2), col = "red", lwd = 2)
text(-.10, x2p*.75 , "Median =", 0)
text(-.045, x2p*.75 , round(median(x2),2), 0)

hist(x3, xlab = 'Growth', main = 'High Volatility Period', xlim=range(full_range_2))
abline(v = median(x3), col = "red", lwd = 2)
text(.07, x3p*.75 , "Median =", 0)
text(.125, x3p*.75 , round(median(x3),2), 0)

hist(y1, xlab = 'Balanced', main = '',  xlim=range(full_range_2))
abline(v = median(y1), col = "red", lwd = 2)
text(-.10, y1p*.75 , "Median =", 0)
text(-.045, y1p*.75 , round(median(y1),2), 0)

hist(y2, xlab = 'Balanced', main = '', xlim=range(full_range_2))
abline(v = median(y2), col = "red", lwd = 2)
text(-.10, y2p*.75 , "Median =", 0)
text(-.045, y2p*.75 , round(median(y2),2), 0)

hist(y3, xlab = 'Balanced', main = '', xlim=range(full_range_2))
abline(v = median(y3), col = "red", lwd = 2)
text(.07, y3p*.75 , "Median =", 0)
text(.125, y3p*.75 , round(median(y3),2), 0)

hist(z1, xlab = 'Conservative', main = '', xlim=range(full_range_2))
abline(v = median(z1), col = "red", lwd = 2)
text(-.10, z1p*.75 , "Median =", 0)
text(-.045, z1p*.75 , round(median(z1),2), 0)

hist(z2, xlab = 'Conservative', main = '', xlim=range(full_range_2))
abline(v = median(z2), col = "red", lwd = 2)
text(-.10, z2p*.75 , "Median =", 0)
text(-.045, z2p*.75 , round(median(z2),2), 0)

hist(z3, xlab = 'Conservative', main = '', xlim=range(full_range_2))
abline(v = median(z3), col = "red", lwd = 2)
text(.07, z3p*.75 , "Median =", 0)
text(.125, z3p*.75 , round(median(z3),2), 0)

s############################
### Summary Statistics  ####
############################

# Returns

# low_vol_sim$annual_return_growth
median(a1)
sd(a1)

# avg_vol_sim$annual_return_growth
median(a2)
sd(a2)

# high_vol_sim$annual_return_growth
median(a3)
sd(a3)

# low_vol_sim$annual_return_balanced
median(b1)
sd(b1)

# avg_vol_sim$annual_return_balanced
median(b2)
sd(b2)

# high_vol_sim$annual_return_balanced
median(b3)
sd(b3)

# low_vol_sim$annual_return_conservative
median(c1)
sd(c1)

# avg_vol_sim$annual_return_conservative
median(c2)
sd(c2)

# high_vol_sim$annual_return_conservative
median(c3)
sd(c3)


# Sharpe Ratios

# low_vol_sim$sharpe_ratio_growth
median(x1)
sd(x1)

# avg_vol_sim$sharpe_ratio_growth
median(x2)
sd(x2)

# high_vol_sim$sharpe_ratio_growth
median(x3)
sd(x3)

# low_vol_sim$sharpe_ratio_balanced
median(y1)
sd(y1)

# avg_vol_sim$sharpe_ratio_balanced
median(y2)
sd(y2)

# high_vol_sim$sharpe_ratio_balanced
median(y3)
sd(y3)

# low_vol_sim$sharpe_ratio_conservative
median(z1)
sd(z1)

# avg_vol_sim$sharpe_ratio_conservative
median(z2)
sd(z2)

# high_vol_sim$sharpe_ratio_conservative
median(z3)
sd(z3)
