## Approximate generation costs by fuel type
## =========================================

## Estimates generation costs from input fuel prices, carbon prices,
## and generation efficiencies.

## Output: A table, with the following columns:



## GDP deflator, market prices
## ---------------------------

## Source: HM Treasury, http://www.hm-treasury.gov.uk/data_gdp_fig.htm
## from ONS National Accounts, 28 March 2011

## By calendar year, 2011 = 100

gdp = data.frame(
  year = 1955:2011,
  deflator = c(4.726, 5.035, 5.225, 5.431, 5.501, 5.572, 5.746, 5.954,
    6.046, 6.263, 6.617, 6.908, 7.111, 7.396, 7.805, 8.392, 9.169,
    9.913, 10.667, 12.255, 15.571, 17.952, 20.41, 22.78, 26.1, 31.2,
    34.749, 37.323, 39.374, 41.184, 43.59, 45.082, 47.493, 50.476,
    54.172, 58.359, 62.129, 64.465, 66.319, 67.368, 69.172, 71.675,
    73.584, 75.059, 76.507, 76.98, 78.095, 80.075, 81.958, 84.001,
    85.838, 88.613, 90.621, 93.462, 95.008, 97.727, 100))

## Average fuel prices, 2002-2010
## ------------------------------

## Source: DECC, Quarterly Energy Prices, Table 3.2.1 (29 March 2012)
## http://www.decc.gov.uk/en/content/cms/statistics/energy_stats/prices/prices.aspx

## 2011 figures are provisional.
## Units:  pence / kWh, nominal.

fuel.price <- data.frame(year = 1992:2011,
                         coal = c(0.660, 0.611,
                           0.528, 0.500, 0.507, 0.474, 0.421, 0.405, 0.406, 0.444, 0.409, 0.389,
                           0.450, 0.497, 0.523, 0.566, 0.929, 0.784,
                           0.901, 1.159),
                         oil = c(0.481, 0.472, 0.526, 0.684, 0.709, 0.746, 0.599, 0.715, 1.010,
                           0.981, 1.061, 1.308, 1.205, 1.932, 2.117, 1.984, 2.373, 2.220, 3.487,
                           4.418),
                         gas = c(NA, 0.706, 0.667, 0.643, 0.628, 0.647, 0.656, 0.613,
                           0.595, 0.664, 0.609, 0.682, 0.761, 1.015, 1.284, 1.236, 1.644, 1.403,
                           1.461, 1.914))

## Cost of Carbon
## --------------

## Source: UK Debt Management Office, result of auction held 8 July
## 2010, press notice (€14.65/tonne). Euro / GBP exchange rate
## (0.8343) taken from same document Units: GBP / tonne CO2

## Source: ICE (www.theice.com)
## ICE-ECX European emissions -- emissions index (futures contract)
## Downloaded 30 April 2012

co2 <- read.csv(file = "historic-co2-prices.csv", header = TRUE)

co2.cost <- 14.65 * 0.8343
fuels$net.price <- fuels$price + fuels$emissions * co2.cost * (100/1000)


## http://www.eex.com/en/Market%20Data/Trading%20Data/Emission%20Rights/EU%20Emission%20Allowances%20%7C%20Spot/EU%20Emission%20Allowances%20Chart%20%7C%20Spot/spot-eua-chart/2012-04-30/1/1/a

## Emissions factors, 2009
## -----------------------

## Source: Defra, August 2011 Guidelines to Defra / DECC's Greenhouse
## Gas Conversion Factors for Company Reporting Gas, Coal (electricity
## generation), and Fuel Oil

## Units:  kg CO_2e (Total Direct GHG) / kWh (Gross Calorific Value)

fuels <- data.frame(row.names = c("Gas", "Coal", "Oil"),
                    price = c(1.461, 0.901, 3.487),
                    emissions = c(0.18360, 0.32518, 0.26744))


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

