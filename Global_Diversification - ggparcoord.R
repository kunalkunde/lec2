### Parallel coordinates plot to see domestic bellwether equity  
### index return in local currency vs. nominal USD vs. real USD 
library(GGally) # to access parallel coordinates function
library(ggalt)
library(ggpmisc)
library(matlab)
library(scales)
library(showtext)
library(tidyverse)
library(xlsx)

# showtext package enables addition of fonts
# font_add(family = "Bookman Old Style", regular = "BOOKOS.TTF")
font_add(family = "Trebuchet MS", regular = "trebuc.ttf")
# font_add(family = "Segoe UI", regular = "segoeui.ttf")
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
Equity_Index_Nominal_USD_Values <- subset(Equity_Index_Nominal_USD_Dates_n_Values, select = -c(Date))
### Calculate dollarized equity index values also adjusted for inflation
Equity_Index_Real_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(subset(Equity_Index_Nominal_USD_Dates_n_Values, select = -c(Date))/subset(Inflation_Conversion_Factors, select = -c(Date)) ) )
colnames(Equity_Index_Real_USD_Dates_n_Values)[1] <- "Date"
Equity_Index_Real_USD_Values <- subset(Equity_Index_Real_USD_Dates_n_Values, select = -c(Date))

lcl_ccy_return_1_mth_PLUS1 <- as.data.frame(Equity_Index_Values/lag(Equity_Index_Values,1))
min_1_mth_lcl_ccy_returns_PLUS1 <- as.data.frame(apply(lcl_ccy_return_1_mth_PLUS1, 2, min, na.rm=TRUE))
min_1_mth_lcl_ccy_ret_indices <- as.data.frame(apply(lcl_ccy_return_1_mth_PLUS1, 2, which.min))
min_1_mth_lcl_ccy_ret_dates <- as.data.frame(Equity_Index_Dates_n_Values$Date[min_1_mth_lcl_ccy_ret_indices[,1]])

Dollarized_lcl_ccy_return_1_mth_PLUS1 <- lcl_ccy_return_1_mth_PLUS1/subset(FX_Conversion_Factors, select = -c(Date))
Dollarized_and_Inf_adj_lcl_ccy_return_1_mth_PLUS1 <- Dollarized_lcl_ccy_return_1_mth_PLUS1/subset(Inflation_Conversion_Factors, select = -c(Date))
Dollarized_lcl_ccy_return_1_mth_contemporaneous <- ones(18,1)
Dollarized_and_Inf_adj_lcl_ccy_return_1_mth_contemporaneous <- ones(18,1)
for (i in seq_len(nrow(min_1_mth_lcl_ccy_ret_indices)))
  {
  Dollarized_lcl_ccy_return_1_mth_contemporaneous[i,1] <- Dollarized_lcl_ccy_return_1_mth_PLUS1[min_1_mth_lcl_ccy_ret_indices[[1]][i],i]-1
  Dollarized_and_Inf_adj_lcl_ccy_return_1_mth_contemporaneous[i,1] <- Dollarized_and_Inf_adj_lcl_ccy_return_1_mth_PLUS1[min_1_mth_lcl_ccy_ret_indices[[1]][i],i]-1
  }

glbl_pf_return_1_mth_PLUS1 <- as.data.frame(Global_Portfolio_Values/lag(Global_Portfolio_Values,1))
glbl_pf_return_1_mth_real_PLUS1 <- glbl_pf_return_1_mth_PLUS1/Inflation_Conversion_Factors$CPI_US
glbl_pf_return_1_mth_contemporaneous <- as.data.frame(glbl_pf_return_1_mth_PLUS1$Global_portfolio_USD[min_1_mth_lcl_ccy_ret_indices[,1]] -1)
glbl_pf_return_1_mth_real_contemporaneous <- as.data.frame(glbl_pf_return_1_mth_real_PLUS1$Global_portfolio_USD[min_1_mth_lcl_ccy_ret_indices[,1]] -1)
Country_Date <- paste(as.vector(rownames(min_1_mth_lcl_ccy_returns_PLUS1)),as.vector(as.character(min_1_mth_lcl_ccy_ret_dates[,1])),sep="\n")
dataForPlot<-data.frame(Country_Date, Min_Lcl_Ccy_Ret=min_1_mth_lcl_ccy_returns_PLUS1[,1]-1, Dollarized_Min_Lcl_Ccy_Ret_Nominal=Dollarized_lcl_ccy_return_1_mth_contemporaneous[,1], Dollarized_Min_Lcl_Ccy_Ret_Real=Dollarized_and_Inf_adj_lcl_ccy_return_1_mth_contemporaneous[,1], Corr_Glbl_Pf_USD_Ret=glbl_pf_return_1_mth_contemporaneous[,1], Corr_Glbl_Pf_USD_Ret_Real=glbl_pf_return_1_mth_real_contemporaneous[,1])
sortedDataForPlot <- dataForPlot[order(dataForPlot$Country_Date, decreasing = FALSE), ]
paletteOf18 <- c("dodgerblue2", "#E31A1C","green4","#6A3D9A","#FF7F00","black", "gold1","skyblue2", "#FB9A99", "palegreen2","#CAB2D6","#FDBF6F","gray70", "khaki2","maroon", "orchid1", "deeppink1", "blue1")
ggparcoord(sortedDataForPlot,columns = 2:4, groupColumn =1, scale="globalminmax", showPoints = TRUE, alphaLines = 1.0)+ theme(text = element_text("Trebuchet MS"))+labs(title="Local currency, Dollarized nominal, and Dollarized real returns (baseline 31-Dec-1999)",x="",y="Worst local currency return over a calendar month") +scale_colour_manual(values = paletteOf18)
