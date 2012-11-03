## This file computes, draws, and annotates an annual price-duration
## curve for the UK wholesale electricity market. It also computes the
## annual profit (revenue from selling electricity less the cost of
## buying it) for two limiting cases of storage.

library("ggplot2")   # charts
library("plyr")      # split-apply-combine analysis
library("scales")    # support for ggplot2
library("grid")      # for unit
library("lubridate") # for dates and times

source("theme_DECC.R")

load("historic-electricity-prices.RData")
## Add useful date fields. 
prices$year     <- year(prices$sett.date)
prices$month    <- month(prices$sett.date, label = TRUE, abbr = FALSE) # Add the names of the months

## Remove NAs; and sort the settlement periods by decreasing price
## within each year.

price_duration <-
  ddply(prices[!is.na(prices$price), c("year", "price")],   
        .(year),  
        transform,
        index = (seq_along(price) - 0.5) / length(price),
        price = sort(price, decreasing = TRUE), 
        .progress = "text")




## PRODUCE PLOTS
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

## On how many days was it profitable to buy and sell?
(ecdf(prices.lowhigh$max/prices.lowhigh$min))(1/storage.eff)


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
                                                 
