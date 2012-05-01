library("plyr")      # split-apply-combine analysis
library("lubridate") # for dates and times


## Generate an R file of electricity market price data
## ===================================================

## Source: Elexon, Market index price data.
##         (see DXX/XXXXXX)
## Units:  GBP/MWh

## Output: A data frame, prices, with the following columns:
##   sett.date    - settlement date
##   sett.period  - settlement period
##   datetime     - start of settlement period as a datetime
##   volume       - volume of energy traded (MWh)
##   price        - price of energy trade (GBP/MWh)

## Notes: The date column is not in a standard format, so is read as a
## character vector and then converted using as.Date()
## mid.vintage <- "../data/Elexon-Prices/2012-02-18"

## Convenience function to load one file of market index data (mid)

load.mid <- function(f) {
  read.csv(file = f,
           header = TRUE,
           col.name = c("d", "sett.period", "MIDP.id", "volume", "price"),
           colClasses = c("character", "integer", "factor", "numeric", "numeric"))
}

## Downloads prior to 2010 contain dates as, eg, "31 December 2009".
## The 2010 download contains dates as, eg, "01-Jan-10".
## Downloads from 2011 onwards contain dates as, eg, "01-Jan-2010".

mid.1 <- rbind(load.mid("../data/Elexon-Prices/2012-02-18/0000001781_mid_2003.csv"),
               load.mid("../data/Elexon-Prices/2012-02-18/0000001782_mid_2004.csv"),
               load.mid("../data/Elexon-Prices/2012-02-18/0000001783_mid_2005.csv"),
               load.mid("../data/Elexon-Prices/2012-02-18/0000001784_mid_2006.csv"),
               load.mid("../data/Elexon-Prices/2012-02-18/0000001785_mid_2007.csv"),
               load.mid("../data/Elexon-Prices/2012-02-18/0000001786_mid_2008.csv"),
               load.mid("../data/Elexon-Prices/2012-02-18/0000001787_mid_2009.csv")
               )
mid.2 <- rbind(load.mid("../data/Elexon-Prices/2012-02-18/0000001788_mid_2010.csv")
               )
mid.3 <- rbind(load.mid("../data/Elexon-Prices/2012-02-18/0000001819_mid_2011.csv"),
               load.mid("../data/Elexon-Prices/2012-02-18/0000003874_mid_2012.csv")
               )

mid.1 <- data.frame(sett.date = as.Date(mid.1$d, format = "%d %B %Y"),
                    subset(mid.1, select = -c(d)))
mid.2 <- data.frame(sett.date = as.Date(mid.2$d, format = "%d-%b-%y"),
                    subset(mid.2, select = -c(d)))
mid.3 <- data.frame(sett.date = as.Date(mid.3$d, format = "%d-%b-%Y"),
                   subset(mid.3, select = -c(d)))

mid <- idata.frame(rbind(mid.1, mid.2, mid.3)) # use immutable dataframe for speed
rm(mid.1, mid.2, mid.3) # Declutter namespace

## Compute average price across all data providers for each settlement
## period. TAKES A LONG TIME TO COMPLETE.

prices <- ddply(mid,
                    .(sett.date, sett.period),
                    summarise, volume = sum(volume), price = weighted.mean(price, volume),
                    .progress = "text"
                    )

## Add the time of the settlement period, taking into account the clock changes. 

prices$datetime <- as.POSIXct(prices$sett.date, tz = "Europe/London") +
  dhours((prices$sett.period - 1) * 0.5)

save(prices, data = "historic-electricity-prices.RData")

