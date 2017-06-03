#install.packages("package")

##################
###    ETL    ####
##################

# The source data is composed of daily total returns by symbol. However,
# the dates are not aligned. This will need to be done in the code.

library(boot)
library(readxl)
library(data.table)
library(ggplot2)
library(stringr)
library(pander)

#file_name <- 'market_data.xlsx'
#file_path <- 'C:\\statistics\\06 statistical programming\\data\\'

file_name <- 'MarketData.xlsx'
file_path <- 'P:\\Library\\Math Class\\MSAS\\APS 7 Stat Programming\\Final Project\\'
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

# Symbol excess return
market_data$spy_excess_return <- (market_data$SPY_Return_Pct - market_data$risk_free_daily_rate)
market_data$tlt_excess_return <- (market_data$TLT_Return_Pct - market_data$risk_free_daily_rate)

# Portolfio Return
market_data$growth_return       <- (.80*market_data$SPY_Return_Pct + .20*market_data$TLT_Return_Pct)
market_data$balanced_return     <- (.65*market_data$SPY_Return_Pct + .35*market_data$TLT_Return_Pct)
market_data$conservative_return <- (.40*market_data$SPY_Return_Pct + .60*market_data$TLT_Return_Pct)

# Portolfio Excess Return
market_data$growth_excess_return       <- (market_data$growth_return       - market_data$risk_free_daily_rate)
market_data$balanced_excess_return     <- (market_data$balanced_return     - market_data$risk_free_daily_rate)
market_data$conservative_excess_return <- (market_data$conservative_return - market_data$risk_free_daily_rate)

# Remove non-trading days
market_data <- market_data[!is.na(SPY_Return_Pct), ]

#########################
##### Check Data  #######
#########################

# The loaded daily returns are compared to known year-end figures.
# This check will verify both the data and the investment return formula.

start_date <- '2015-01-01'
stop_date  <- '2015-12-31'
market_data_cut <- market_data[Date >= start_date & Date <= stop_date, ]

# Checking for missing data
market_data[Date >= start_date & Date <= stop_date & is.na(TLT_Return_Pct),]
market_data[Date >= start_date & Date <= stop_date & is.na(SPY_Return_Pct),]
market_data[Date >= start_date & Date <= stop_date & is.na(USGG1M_Return_Pct),] # Missing value on '2015-05-08'

# Verify SPY 2015 Return
market_data_cut$SPY_Return_Index <- (market_data_cut$SPY_Return_Pct / 100) + 1
annual_return <- ( prod(market_data_cut$SPY_Return_Index, na.rm = FALSE) - 1)*100
annual_return
# Expecting 1.25 [http://performance.morningstar.com/funds/etf/total-returns.action?t=SPY]

# Verify TLT 2015 Return
market_data_cut$TLT_Return_Index <- (market_data_cut$TLT_Return_Pct / 100) + 1
annual_return <- ( prod(market_data_cut$TLT_Return_Index, na.rm = FALSE) - 1)*100
annual_return
# Expecting -1.79 [http://performance.morningstar.com/funds/etf/total-returns.action?t=TLT]

################################
##### Define Time Periods  #####
################################

# Creat time periods to represent high, low, and average volatility regimes.
# Use market_data as source data file.

# Average volatility (10 years): 04/01/2007 - 03/31/2017
start_date <- '2007-04-01'
stop_date  <- '2017-03-31'

avg_vol_days <- market_data[Date >= start_date & Date <= stop_date & Date != '2015-05-08', Date]
avg_vol_day_count <- length(avg_vol_days)
avg_vol_day_count
## There are 2,518 Days in Average Volatility time period

# High volatility (VIX >30) 9/15/08 - 5/18/09
start_date <- '2008-09-15'
stop_date  <- '2009-05-18'

high_vol_days <- market_data[Date >= start_date & Date <= stop_date, Date]
high_vol_day_count <- length(high_vol_days)
high_vol_day_count
## There are 170 Days in High Volatility time period

# Low volatility 11/01/04 - 1/1/07 + 1/4/2013 - 10/3/14
start_date1 <- '2004-11-01'
stop_date1  <- '2007-01-01'
start_date2 <- '2013-01-04'
stop_date2  <- '2014-10-03'

range1 <- market_data[Date >= start_date1 & Date <= stop_date1, Date]
range2 <- market_data[Date >= start_date2 & Date <= stop_date2, Date]

low_vol_days <- market_data[Date %in% c(range1,range2),Date] 
low_vol_day_count <- length(low_vol_days)
low_vol_day_count
## There are 987 Days in Low Volatility time period

############################
### Normal Simulation  #####
############################

# Actual returns
SPY_daily_returns <- market_data[Date %in% avg_vol_days,SPY_Return_Pct]

# Normal Simulation
SPY_mean <- mean(SPY_daily_returns)
SPY_sd   <- sd(SPY_daily_returns)
SPY_sim_returns <- rnorm(avg_vol_day_count,SPY_mean,SPY_sd)

# Two-sample Kolmogorov-Smirnov test
# Do x and y come from the same distribution?
ks.test(SPY_daily_returns, SPY_sim_returns)

par(mfrow=c(1,1))
plot(ecdf(SPY_daily_returns), xlim=range(c(SPY_daily_returns, SPY_sim_returns)), col = c("blue"), 
     main="Empirical Distribution Function (ECDF)", xlab="Daily Return", ylab="Cumulative Probability")
plot(ecdf(SPY_sim_returns), add=TRUE, lty=1, col = c("red"))
legend("bottomright",
        legend=c("SPY Actual Returns","SPY Simulated Returns"),
        col = c("blue", "red"), lwd=1, lty=c(1,1 ))

# There is significant evidence (p-value < .0001) that the normally simulated SPY returns and the 
# actual SPY returns are not from the same distribution. In other words, the observed returns are 
# not normally distributed. The normal distribution is a poor source for generating stock returns.

##############################
### Bootstrap Simulation  ####
##############################

# A simulation is run where data is sampled from the specified time period.
# One simulation is equivelent to one 10 year period of trading days.
# Time periods include (i) avg_vol_days (ii) high_vol_days and (iii) low_vol_days

# Bootstrap parameters
loops <- 2500                # Number of simulations loops
days  <- avg_vol_day_count  # Days in 10 year time period

# Simulation Function
ret.fun = function(x){  
  one_sample <- data.table(Date=sample(x, days, replace=T), key="Date")    # Generate sample dates, 'days' trading days per loop
  result <- merge(one_sample,market_data,all.x = TRUE)                               # Join in market data
  result$growth_Return_Index        <- (result$growth_return / 100) + 1                       # Calculate return index
  result$balanced_Return_Index      <- (result$balanced_return / 100) + 1
  result$conservative_Return_Index  <- (result$conservative_return / 100) + 1
  
  annual_return_growth       <- (prod(result$growth_Return_Index, na.rm = FALSE) - 1)*100        # Calculate annulized return
  annual_return_balanced     <- (prod(result$balanced_Return_Index, na.rm = FALSE) - 1)*100
  annual_return_conservative <- (prod(result$conservative_Return_Index, na.rm = FALSE) - 1)*100
  
  sharpe_ratio_growth       <- ( mean(result$growth_excess_return)   / sd(result$growth_excess_return       )) #/ (days^(1/2))
  sharpe_ratio_balanced     <- ( mean(result$balanced_excess_return) / sd(result$balanced_excess_return     )) #/ (days^(1/2))
  sharpe_ratio_conservative <- ( mean(result$conservative_return)    / sd(result$conservative_excess_return )) #/ (days^(1/2))

  return(Map(cbind, annual_return_growth, annual_return_balanced, annual_return_conservative, 
             sharpe_ratio_growth, sharpe_ratio_balanced, sharpe_ratio_conservative))  
}

# Format Output Function
fmat.fun = function(x){  
  sim_df <- as.data.frame(matrix(unlist(x), nrow=length(x), ncol=length(x[[1]]), byrow = TRUE))
  a <- cbind.data.frame(group="Growth"      , return=sim_df[,1], sharpe=sim_df[,4])
  b <- cbind.data.frame(group="Balanced"    , return=sim_df[,2], sharpe=sim_df[,5])
  c <- cbind.data.frame(group="Conservative", return=sim_df[,3], sharpe=sim_df[,6])
  formatted_output <- data.table(as.data.frame(rbind.data.frame(a,b,c)))
  return(formatted_output)
}

# Average Volatility Simulation: Returns and Sharpe Ratios
avg_sim <- replicate(loops, ret.fun(avg_vol_days), simplify = "array")
avg_sim_out <- fmat.fun(avg_sim)
print(avg_sim_out[1:5,])

# High Volatility Simulation: Returns and Sharpe Ratios
hi_sim <- replicate(loops, ret.fun(high_vol_days), simplify = "array")
hi_sim_out <- fmat.fun(hi_sim)
print(hi_sim_out[1:5,])

# Low Volatility Simulation: Returns and Sharpe Ratios
low_sim <- replicate(loops, ret.fun(low_vol_days), simplify = "array")
low_sim_out <- fmat.fun(low_sim)
print(low_sim_out[1:5])

#########################
### Summary Stat Calcs ##
#########################

a <- cbind.data.frame(volatility="Average", avg_sim_out)
b <- cbind.data.frame(volatility="High"   , hi_sim_out)
c <- cbind.data.frame(volatility="Low"    , low_sim_out)
output.merged.dt <- data.table(as.data.frame(rbind.data.frame(a,b,c)))

returns.dt <- output.merged.dt[,.("return.mean"=mean(return), "return.median"=median(return), "return.sd"=sd(return)), by=.(volatility,group)]

sharpe.dt <- output.merged.dt[,.("sharpe.mean"=mean(sharpe), "sharpe.median"=median(sharpe), "sharpe.sd"=sd(sharpe)), by=.(volatility,group)]

######################
### Histograms    ####
######################

# return.hist.fun("Volatility",binwidth)
return.hist.fun = function(v,w){ 
  z <- ggplot(output.merged.dt[volatility==v], aes(x=return)) + geom_histogram(binwidth=w, colour="black", fill="white") + 
    facet_grid(group ~ .) + ggtitle(paste(v,"Volatility")) + xlab("10 Year Annual Return") +
    geom_vline(data=returns.dt[volatility==v], aes(xintercept=return.median), linetype="dashed", size=1, colour="red")
  return(z)
}

# sharpe.hist.fun("Volatility",binwidth)
sharpe.hist.fun = function(v,w){ 
  z <- ggplot(output.merged.dt[volatility==v], aes(x=sharpe)) + geom_histogram(binwidth=w, colour="black", fill="white") + 
    facet_grid(group ~ .) + ggtitle(paste(v,"Volatility")) + xlab("10 Year Sharpe Ratio") +
    geom_vline(data=sharpe.dt[volatility==v], aes(xintercept=sharpe.median), linetype="dashed", size=1, colour="red")
  return(z)
}

return.hist.fun("Average",10)
return.hist.fun("High"   ,3.5)
return.hist.fun("Low"    ,5)

sharpe.hist.fun("Average",.002)
sharpe.hist.fun("High"   ,.002)
sharpe.hist.fun("Low"    ,.002)

######################
### Summary Stats   ##
######################

panderOptions('keep.trailing.zeros', TRUE)
panderOptions('round', 1)
pander(returns_df)
panderOptions('round', 3)
pander(sharpe_df)
