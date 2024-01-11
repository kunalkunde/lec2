### Dumbbell plot to see lcl-vs-glbl performance
library(ggalt)
library(ggpmisc)
library(ggrepel)
library(scales)
library(showtext)
library(tidyverse)
library(xlsx)

# showtext package enables addition of fonts
#font_add(family = "Bookman Old Style", regular = "BOOKOS.TTF")
font_add(family = "Trebuchet MS", regular = "trebuc.ttf")
#font_add(family = "Segoe UI", regular = "segoeui.ttf")
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

### Read in global portfolio values based on yearly re-balance to MSCI ACWI weights (every December-end)
Global_Portfolio_Dates_n_Values <- read.xlsx("Global_portfolio_USD.xlsx", sheetName="Global_portfolio_USD")
Global_Portfolio_Values <- subset(Global_Portfolio_Dates_n_Values, select = -c(Date))
# these are USD values and their computation is laid out within the "Mthly eq indx data" sheet of the following Excel source file (i.e., the above Excel file being read from is just a values-only record): 
# https://iiflw-my.sharepoint.com/personal/kunal_kunde_iiflw_com/Documents/WIP/International%20FOF/2021.12.07.BQL%20wkly%20lcl%20values%20since%20Xmas%201999%20for%2018%20glbl%20eq%20indices.xlsx?web=1

### Calculate conversion factors
Inflation_Conversion_Factors <- data.frame(Inflation_Index_Dates_n_Values$Date,t(t(Inflation_Index_Values)/t(Inflation_Index_Values)[,1])) 
colnames(Inflation_Conversion_Factors)[1] <- "Date"
# second arg in the data.frame() constructor above is a scaling that can be done only via column division, hence successive transposes on the RHS
# source = https://www.geeksforgeeks.org/divide-each-row-of-matrix-by-vector-elements-in-r/
Equity_Conversion_Factors <- data.frame(Equity_Index_Dates_n_Values$Date,t(t(Equity_Index_Values)/t(Equity_Index_Values)[,1])) 
colnames(Equity_Conversion_Factors)[1] <- "Date"
FX_Conversion_Factors <- data.frame(FX_Rate_Dates_n_Values$Date,t(t(FX_Rate_Values)/t(FX_Rate_Values)[,1])) 
colnames(FX_Conversion_Factors)[1] <- "Date"
Global_Portfolio_Conversion_Factors <- data.frame(Global_Portfolio_Dates_n_Values$Date,t(t(Global_Portfolio_Values)/t(Global_Portfolio_Values)[,1])) 
colnames(Global_Portfolio_Conversion_Factors)[1] <- "Date"

### Calculate dollarized equity index values
Equity_Index_Nominal_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(Equity_Index_Values/FX_Rate_Values))
colnames(Equity_Index_Nominal_USD_Dates_n_Values)[1] <- "Date"
### Calculate dollarized equity index values also adjusted for inflation
Equity_Index_Real_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(subset(Equity_Index_Nominal_USD_Dates_n_Values, select = -c(Date))/subset(Inflation_Conversion_Factors, select = -c(Date)) ) )
colnames(Equity_Index_Real_USD_Dates_n_Values)[1] <- "Date"

listOfDataFrames <- list(Equity_Index_Dates_n_Values, FX_Rate_Dates_n_Values, Inflation_Index_Dates_n_Values, Global_Portfolio_Dates_n_Values, Equity_Index_Nominal_USD_Dates_n_Values, Equity_Index_Real_USD_Dates_n_Values)
names(listOfDataFrames)=c("EQ","FX","CPI","GlblPf","EQ_nom_USD","EQ_real_USD")
nMths <- seq(from = 1, to = 120, by = 1)

plotLclGlblDumbbellPlots <- function(numMths, listOfDataSets){
Equity_Index_Values <- subset(listOfDataSets[[1]], select = -c(Date))
lcl_ccy_return_n_mth <- as.data.frame(Equity_Index_Values/lag(Equity_Index_Values,numMths)-1)
min_n_mth_lcl_ccy_returns <- as.data.frame(apply(lcl_ccy_return_n_mth, 2, min, na.rm=TRUE))
min_n_mth_lcl_ccy_ret_indices <- as.data.frame(apply(lcl_ccy_return_n_mth, 2, which.min))
min_n_mth_lcl_ccy_ret_dates <- as.data.frame(listOfDataSets[[1]]$Date[min_n_mth_lcl_ccy_ret_indices[,1]])
Global_Portfolio_Values <- subset(listOfDataSets[[4]], select = -c(Date))
glbl_pf_return_n_mth <- as.data.frame(Global_Portfolio_Values/lag(Global_Portfolio_Values,numMths)-1)
glbl_pf_return_n_mth_contemporaneous <- as.data.frame(glbl_pf_return_n_mth$Global_portfolio_USD[min_n_mth_lcl_ccy_ret_indices[,1]])
Country_Date <- paste(as.vector(rownames(min_n_mth_lcl_ccy_returns)),as.vector(as.character(min_n_mth_lcl_ccy_ret_dates[,1])),sep="\n")
dataForPlot<-data.frame(Country_Date, Min_Lcl_Ccy_Ret=min_n_mth_lcl_ccy_returns[,1], Corr_Glbl_Pf_USD_Ret=glbl_pf_return_n_mth_contemporaneous[,1])
graphTitle <- sprintf("Worst return over %d cal. months, local (red) vs. global (blue), Dec 1999 to Dec 2021",numMths)
#ggplot() + geom_dumbbell(data = dataForPlot, aes(y=Country_Date, x=Min_Lcl_Ccy_Ret, xend=Corr_Glbl_Pf_USD_Ret),size = 0.5, size_x = 1.5, size_xend = 1.5,colour_x = "red", colour_xend = "blue") + theme(text = element_text("Trebuchet MS")) + labs(title = graphTitle) + geom_text(color="red", size=4, hjust=0, vjust=-1, aes(x=dataForPlot$Min_Lcl_Ccy_Ret, y=dataForPlot$Country_Date, label=format(scales::percent(dataForPlot$Min_Lcl_Ccy_Ret, accuracy=.1, trim = FALSE),nsmall=2))) + geom_text(color="blue", size=4, hjust=0, vjust=-1, aes(x=dataForPlot$Corr_Glbl_Pf_USD_Ret, y=dataForPlot$Country_Date, label=format(scales::percent(dataForPlot$Corr_Glbl_Pf_USD_Ret, accuracy=.1, trim = FALSE),nsmall=2)))+xlab("Country index lcl ccy ret. (red) & Global pf USD ret. (blue)") +ylab("Country & period-ending date (YYYY-MM-DD)")
ggplot() + geom_dumbbell(data = dataForPlot, aes(y=Country_Date, x=Min_Lcl_Ccy_Ret, xend=Corr_Glbl_Pf_USD_Ret),size = 0.5, size_x = 1.5, size_xend = 1.5,colour_x = "red", colour_xend = "blue") + theme(text = element_text("Trebuchet MS")) + labs(title = graphTitle) + geom_text(color="red", size=4, hjust=1, vjust=1, aes(x=dataForPlot$Min_Lcl_Ccy_Ret, y=dataForPlot$Country_Date, label=format(scales::percent(dataForPlot$Min_Lcl_Ccy_Ret, accuracy=.1, trim = FALSE),nsmall=2))) + geom_text(color="blue", size=4, hjust=0, vjust=1, aes(x=dataForPlot$Corr_Glbl_Pf_USD_Ret, y=dataForPlot$Country_Date, label=format(scales::percent(dataForPlot$Corr_Glbl_Pf_USD_Ret, accuracy=.1, trim = FALSE),nsmall=2)))+xlab("Country index lcl ccy ret. (red) & Global pf USD ret. (blue)") +ylab("Country & period-ending date (YYYY-MM-DD)")
}

containerForPlots <- map(nMths, ~plotLclGlblDumbbellPlots(.x, listOfDataSets = listOfDataFrames))
