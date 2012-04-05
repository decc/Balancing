## This file computes, draws, and annotates an annual price-duration
## curve for the UK wholesale electricity market

library("ggplot2")   # charts
library("plyr")      # split-apply-combine analysis
library("scales")    # support for ggplot2
library("grid")      # for unit
library("lubridate") # for dates and times

source("theme_DECC.R")


## Load price data and transform to usable format.
## ===============================================

## Market index price data for 2010.
##
## The date column is not in a standard format, so is read as a
## character vector and then converted using as.Date()
##
## Source: Elexon (see DXX/XXXXXX)
## Units: GBP/MWh

# mid.vintage <- "../data/Elexon-Prices/2012-02-18"

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

## Add useful date fields. 
prices$year     <- year(prices$sett.date)
prices$datetime <- as.POSIXct(prices$sett.date, tz = "Europe/London") +
  dhours((prices$sett.period - 1) * 0.5)
prices$month    <- month(prices$sett.date, label = TRUE, abbr = FALSE) # Add the names of the months

## Sort the settlement periods by decreasing price within each year 

price_duration <- ddply(prices[!is.na(prices$price), c("year", "price")],   # remove NAs ...
                        .(year),  # ... and split by year
                        transform,
                        index = (seq_along(price) - 0.5) / length(price), # add a settlement period index within each year 
                        price = sort(price, decreasing = TRUE), # and sort by price
                        .progress = "text")


## Compute marginal costs of production in 2010
## ============================================

## Average fuel prices, 2010
## -------------------------
## Source: DECC, Quarterly Energy Prices, Table 3.2.1 (December 2011)
## http://www.decc.gov.uk/en/content/cms/statistics/energy_stats/prices/prices.aspx
## Units:  pence / kWh

## Emissions factors, 2009
## -----------------------

## Source: Defra, August 2011 Guidelines to Defra / DECC's Greenhouse
## Gas Conversion Factors for Company Reporting
## Gas, Coal (electricity generation), and Fuel Oil
## Units:  kg CO_2e (Total Direct GHG) / kWh (Gross Calorific Value)

fuels <- data.frame(row.names = c("Gas", "Coal", "Oil"),
                    price = c(1.461, 0.901, 3.487),
                    emissions = c(0.18360, 0.32518, 0.26744))

## Cost of Carbon, 2010
## --------------------

## Source: UK Debt Management Office, result of auction held 8 July
## 2010, press notice (€14.65/tonne). Euro / GBP exchange rate
## (0.8343) taken from same document Units: GBP / tonne CO2

co2.cost <- 14.65 * 0.8343
fuels$net.price <- fuels$price + fuels$emissions * co2.cost * (100/1000)

## Power station efficiencies, 2010
## --------------------------------

## Source: DUKES 2011, Table 5.10 and others (see below)
## Units: % (by Gross Calorific Value)

## Assumes coal generation own-use is 5%; CCGT / OCGT is 1%; (These
## are approximate figures computed from electricity fuel use,
## generation and supply, Table 5.6). Oil efficiencies computed
## directly from Table 5.6.

## OCGT efficiency of 41% taken from Parsons Brinckerhoff, Energy
## generation cost model -- 2011 Update Revision 1, August 2011,
## central estimate of Nth-of-a-kind. Refers to Net Calorific Value,
## assumed to be 90% of GCV.

generation <- data.frame(row.names = c("Coal", "CCGT", "OCGT", "Oil"),
                         type = c("Coal", "CCGT", "OCGT", "Oil"),
                         efficiency = c(
                           0.361 * (1-0.05),
                           0.476 * (1-0.01), 
                           0.41 * 0.9,
                           0.319))

generation[, "cost"] <- fuels[c("Coal", "Gas", "Gas", "Oil"),
                              "net.price"] / generation[, "efficiency"]

generation[, "label.x"] = rep(0.7, 4)
generation[, "label.y"] = c(90, 60, 120, 150)


## Produce plots
## =============

## Set up output device
quartz(width = 105/25.4, height = 105/25.4/sqrt(2), pointsize = 10,
       family = "Helvetica", type = "pdf", file = "pd.pdf")

## Plot price_duration curve
qplot(data = price_duration[price_duration$year == 2010, ],
      index, price, geom = "line", size = I(0.7),
      xlab = "Fraction of year", ylab = "Wholesale price (\u{00A3} / MWh)") +
  xlim(0,1) +
  theme_DECC() +
  geom_segment(aes(x = 0.45, y = cost*10, xend = 0.55, yend = cost*10),
               data = generation,
               colour = DECC.colours$cyan, size = 0.35) +
  geom_segment(aes(x = 0.55, y = cost*10, xend = label.x, yend =
      label.y),
               data = generation,
               colour = DECC.colours$cyan, size = 0.1) +
  geom_text(aes(x = label.x+0.01, y = label.y, label = type),
            data = generation,
            size = 8/72.27*25.4, colour = DECC.colours$cyan, family =
            "Helvetica Light",
            vjust = 0, hjust = 0)

dev.off()



## CALCULATE MAXIMUM EXTRACTABLE VALUE
## ===================================

storage.eff <- 0.75 # Round-trip efficiency

## Rate-limited storage
## --------------------

## Assumes 1 MW power, so 0.5 MWh energy per settlement period.

values <- ddply(price_duration,
                .(year),
                function (pd) {
                  pd$ecirp <- sort(pd$price, decreasing = FALSE) # Order prices from low to high
                  pd$profit <- storage.eff * pd$price - pd$ecirp # Buy low, sell high ...
                  total.value <- sum(pd$profit[pd$profit > 0]) # ... but only while you're making a profit
                  c(value = 0.5 * total.value) # Factor of 0.5 because
                                               # can buy or sell 0.5
                                               # MWh per period.
                },
                .progress = "text")

               
## Volume-limited storage
## ----------------------

## Assumes 1 MWh storage (so that 1 MWh may be stored
## per settlement period). Note that this requires a minimum 2 MW power.

## Max and min prices by day
prices.lowhigh <- ddply(prices,
                        .(sett.date),
                        summarise, max = max(price, na.rm = TRUE), min = min(price, na.rm = TRUE),
                        .progress = "text")

prices.lowhigh <- transform(prices.lowhigh,
                            year = year(sett.date),
                            profit = pmax(0, storage.eff * max - min))

values <- ddply(prices.lowhigh,
                .(year),
                summarise, total = sum(profit),
                .progress = "text")

## Plot of minimum and maximum prices by day
## -----------------------------------------

quartz(width = 140/25.4, height = 140/25.4/sqrt(2), pointsize = 10,
       family = "Helvetica", type = "pdf", file = "minmax.pdf")

ggplot(prices.lowhigh, aes(sett.date)) +
  scale_y_continuous(name = "Price (\u{00A3} / MWh)", limits = c(0,600), expand = c(0,0)) +
  scale_x_date(name = "Date", breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  geom_line(aes(y = min), size = 0.2, colour = DECC.colours$cyan) +
  geom_line(aes(y = max), size = 0.2, colour = DECC.colours$orange) +
  theme_DECC()

dev.off()


## Other plots
## ===========

## Price-duration plot for each year
qplot(data = price_duration,
      index, price, geom = "line", size = I(0.7), colour = as.factor(year),
      xlab = "Fraction of year", ylab = "Wholesale price (GBP / MWh)") +
  scale_x_continuous(trans = "logit") + scale_y_log10() +
  scale_colour_brewer(palette = "Spectral") + theme_DECC()

## Daily prices for each month in 2010
quartz(width = 400/25.4, height = 400/25.4/sqrt(2), pointsize = 16,
       family = "Helvetica", type = "pdf", file = "allprices.pdf")

qplot(sett.period * 0.5, price, data = subset(prices, (year(sett.date) == 2010)),
      group = yday(sett.date),
      geom = "line",
#      ylim = c(0,500),
      alpha = I(0.15), colour = I(DECC.colours$cyan),
      main = "Daily electricity prices 2010",
      xlab = "Time of day",
      ylab = "Wholesale price (\u{00A3} / MWh)") + facet_wrap(~ month) + theme_DECC()

dev.off()

## Another approach to plotting the price range
ggplot(prices, aes(x = sett.date, y = price)) +
  scale_y_continuous(name = "Price (GBP/MWh)") +
  scale_x_date(name = "Date", breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  stat_summary(fun.data = "median_hilow", colour = hex(mixcolor(0.8, RGB(1,1,1), hex2RGB(DECC.colours$cyan))), 
               geom = "linerange", width = 0.2) +
  stat_summary(fun.y = "median", colour = DECC.colours$orange, geom = "line", size = 0.2) +
  theme_DECC()
               
qplot(sett.date, max/min, data = prices.lowhigh, size = I(1)) + geom_smooth(method = "loess", span = 0.1, se = FALSE)
                                                 
