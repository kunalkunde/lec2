### This R script started its life as a child of the  
#   <Global_Diversification - geom_dumbbell map.R> parent.
### It has been created to make line graphs of equity indices
#   and conversion factors for equities, FX rates, and CPI time series.
library(ggalt)
library(ggplotify)
library(ggpmisc)
library(gridExtra)
library(scales)
library(showtext)
library(tidyverse)
library(xlsx)

# showtext package enables addition of fonts
font_add(family = "Trebuchet MS", regular = "trebuc.ttf")
showtext_auto() # makes fonts visible to ggplot

### Set working directory
setwd("C:/Users/kunal.kunde/OneDrive - iifl wealth/KKK/R/Global_Diversification")

### Read in equity, forex and inflation data
Equity_Index_Dates_n_Values <- read.xlsx("2021.12.15.Local Global real return comparison.xlsx", sheetName="Equity_index_values")
Equity_Index_Values <- subset(Equity_Index_Dates_n_Values, select = -c(Date))
FX_Rate_Dates_n_Values <- read.xlsx("2021.12.15.Local Global real return comparison.xlsx", sheetName="FX_rate_values")
FX_Rate_Values <- subset(FX_Rate_Dates_n_Values, select = -c(Date))
Inflation_Index_Dates_n_Values <- read.xlsx("2021.12.15.Local Global real return comparison.xlsx", sheetName="Inflation_index_values")
Inflation_Index_Values <- subset(Inflation_Index_Dates_n_Values, select = -c(Date))
### Calculate corresponding conversion factors
Inflation_Conversion_Factors <- data.frame(Inflation_Index_Dates_n_Values$Date,t(t(Inflation_Index_Values)/t(Inflation_Index_Values)[,1]))
colnames(Inflation_Conversion_Factors)[1] <- "Date"
Equity_Conversion_Factors <- data.frame(Equity_Index_Dates_n_Values$Date,t(t(Equity_Index_Values)/t(Equity_Index_Values)[,1])) 
colnames(Equity_Conversion_Factors)[1] <- "Date"
FX_Conversion_Factors <- data.frame(FX_Rate_Dates_n_Values$Date,t(t(FX_Rate_Values)/t(FX_Rate_Values)[,1]))
colnames(FX_Conversion_Factors)[1] <- "Date"

### Calculate dollarized equity index values
Equity_Index_Nominal_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(Equity_Index_Values/FX_Rate_Values))
colnames(Equity_Index_Nominal_USD_Dates_n_Values)[1] <- "Date"
### Calculate dollarized equity index values also adjusted for inflation
Equity_Index_Real_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(subset(Equity_Index_Nominal_USD_Dates_n_Values, select = -c(Date))/subset(Inflation_Conversion_Factors, select = -c(Date)) ) )
colnames(Equity_Index_Real_USD_Dates_n_Values)[1] <- "Date"

listOfDataFrames <- list(Equity_Index_Dates_n_Values, FX_Rate_Dates_n_Values, Inflation_Index_Dates_n_Values, Equity_Index_Nominal_USD_Dates_n_Values, Equity_Index_Real_USD_Dates_n_Values)
countryNames <- c("Australia", "Japan", "China", "Canada","Switzerland", "USA", "UK", "HK", "France", "Germany", "South Korea", "Brazil", "Ireland", "Taiwan", "India", "Netherlands", "Russia", "South Africa")
currencyNames <- c("AUD", "JPY", "CNY", "CAD","CHF", "USD", "GBP", "HKD", "EUR", "EUR", "KRW", "BRL", "EUR", "TWD", "INR", "EUR", "RUB", "ZAR")
eqIndexNames <- c("ASX200", "Nikkei", "Shanghai Comp.", "TSX Comp.","SMI", "S&P 500", "FTSE 100", "Hang Seng", "CAC 40", "DAX", "Kospi", "Bovespa", "ISEQ", "Taiex", "Nifty 50", "AEX", "MOEX", "Top 40")
names(listOfDataFrames)=c("EQ","FX","CPI","EQ_nom_USD","EQ_real_USD")
indexNumber <- seq(from = 2, to = 19, by = 1)

plotLineGraphs <- function(index,listOfDataSets){
dataForPlot <- data.frame(listOfDataFrames[[1]][1], listOfDataFrames[[1]][index], listOfDataFrames[[2]][index], listOfDataFrames[[4]][index], listOfDataFrames[[5]][index])

#1 graphTitle <- sprintf("%s index for %s in %s", eqIndexNames[index-1], countryNames[index-1], currencyNames[index-1])
#1 colnames(dataForPlot)[2] <- "Index_lcl_ccy"
#1 ggplot(data = dataForPlot, aes(x=Date)) + geom_line(aes(y=Index_lcl_ccy), color = "hotpink", size=1.5) + theme(text = element_text("Trebuchet MS")) + labs(title = graphTitle) + ylab("Index")

#2 graphTitle <- sprintf("%s index for %s in \nspot USD (brown) and in 1999 USD (black)", eqIndexNames[index-1], countryNames[index-1])
#2 colnames(dataForPlot)[4] <- "Index_in_spot_USD"
#2 colnames(dataForPlot)[5] <- "Index_in_1999_USD"
#2 ggplot(data = dataForPlot, aes(x=Date)) + geom_line(aes(y=Index_in_spot_USD), color = "chocolate4", size=1.5) + geom_line(aes(y=Index_in_1999_USD), color = "gray10", size=1.5) + theme(text = element_text("Trebuchet MS")) + labs(title = graphTitle) + ylab("Index")

graphTitle <- sprintf("%s - %s versus USD \n(lcl ccy units per USD)", countryNames[index-1], currencyNames[index-1])
colnames(dataForPlot)[3] <- "FX_rate"
ggplot(data = dataForPlot, aes(x=Date)) + geom_line(aes(y=FX_rate), color = "green", size=1.5) + theme(text = element_text("Trebuchet MS")) + labs(title = graphTitle) + ylab("FX rate (lcl ccy units per USD)")
}

containerForLineGraphs <- map(indexNumber, ~plotLineGraphs(.x, listOfDataSets = listOfDataFrames))

# testPlot01 <- as.grob(containerForLineGraphs[[1]])
# testPlot02 <- as.grob(containerForLineGraphs[[2]])
# testPlot03 <- as.grob(containerForLineGraphs[[3]])
# testPlot04 <- as.grob(containerForLineGraphs[[4]])
# testPlot05 <- as.grob(containerForLineGraphs[[5]])
# testPlot06 <- as.grob(containerForLineGraphs[[6]])
# testPlot07 <- as.grob(containerForLineGraphs[[7]])
# testPlot08 <- as.grob(containerForLineGraphs[[8]])
# testPlot09 <- as.grob(containerForLineGraphs[[9]])
# testPlot10 <- as.grob(containerForLineGraphs[[10]])
# testPlot11 <- as.grob(containerForLineGraphs[[11]])
# testPlot12 <- as.grob(containerForLineGraphs[[12]])
# testPlot13 <- as.grob(containerForLineGraphs[[13]])
# testPlot14 <- as.grob(containerForLineGraphs[[14]])
# testPlot15 <- as.grob(containerForLineGraphs[[15]])
# testPlot16 <- as.grob(containerForLineGraphs[[16]])
# testPlot17 <- as.grob(containerForLineGraphs[[17]])
# testPlot18 <- as.grob(containerForLineGraphs[[18]])
# grid01 <- grid.arrange(testPlot01, testPlot12, testPlot04, testPlot03, testPlot09, testPlot10, testPlot08, testPlot15, testPlot13, nrow=3)
# grid02 <- grid.arrange(testPlot02, testPlot16, testPlot17, testPlot18, testPlot11, testPlot05, testPlot14, testPlot07, testPlot06, nrow=3)
# ggsave("grid01.png", grid01, width=14, height=8.50, units="in", scale=1)
# ggsave("grid02.png", grid02, width=14, height=8.50, units="in", scale=1)

