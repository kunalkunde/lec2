### Log [nMths, Country_ccy, Date, localPerf, globalPerf] records,
### then make grids of XY scatterplots by country for worst  
### performance over {1,2,3,...118,119,120} calendar months
library(ggalt)
library(ggplotify)
library(ggpmisc)
library(gridExtra)
library(scales)
library(showtext)
library(tidyverse)
library(xlsx)

### "showtext" package enables font_add() for addition of fonts
font_add(family = "Trebuchet MS", regular = "trebuc.ttf")
showtext_auto() # makes fonts visible to ggplot
### Set working directory
setwd("C:/Users/kunal.kunde/OneDrive - iifl wealth/KKK/R/Global_Diversification")
### Read in equity, forex and inflation data
Equity_Index_Dates_n_Values <- read.xlsx("2021.12.15.Local Global real return comparison.xlsx", sheetName="Equity_index_values")
### Read in global portfolio values based on yearly re-balance to MSCI ACWI weights (every December-end)
Global_Portfolio_Dates_n_Values <- read.xlsx("Global_portfolio_USD.xlsx", sheetName="Global_portfolio_USD")
# these are USD values and their computation is laid out within the "Mthly eq indx data" sheet of the following Excel source file (i.e., the above Excel file being read from is just a values-only record): 
# https://iiflw-my.sharepoint.com/personal/kunal_kunde_iiflw_com/Documents/WIP/International%20FOF/2021.12.07.BQL%20wkly%20lcl%20values%20since%20Xmas%201999%20for%2018%20glbl%20eq%20indices.xlsx?web=1

listOfDataFrames <- list(Equity_Index_Dates_n_Values, Global_Portfolio_Dates_n_Values)
names(listOfDataFrames)=c("EQ","GlblPf")
nMths <- seq(from = 1, to = 120, by = 1)
myListForLogs <- list()

logLclGlblDataPoints <- function(numMths, listOfDataSets){
Equity_Index_Values <- subset(listOfDataSets[[1]], select = -c(Date))
lcl_ccy_return_n_mth <- as.data.frame(Equity_Index_Values/lag(Equity_Index_Values,numMths)-1)
min_n_mth_lcl_ccy_returns <- as.data.frame(apply(lcl_ccy_return_n_mth, 2, min, na.rm=TRUE))
min_n_mth_lcl_ccy_ret_indices <- as.data.frame(apply(lcl_ccy_return_n_mth, 2, which.min))
min_n_mth_lcl_ccy_ret_dates <- as.data.frame(listOfDataSets[[1]]$Date[min_n_mth_lcl_ccy_ret_indices[,1]])
Global_Portfolio_Values <- subset(listOfDataSets[[2]], select = -c(Date))
glbl_pf_return_n_mth <- as.data.frame(Global_Portfolio_Values/lag(Global_Portfolio_Values,numMths)-1)
glbl_pf_return_n_mth_contemporaneous <- as.data.frame(glbl_pf_return_n_mth$Global_portfolio_USD[min_n_mth_lcl_ccy_ret_indices[,1]])
dataForLog<-data.frame(nMonths=c(rep(numMths,18)),Country_Ccy=as.vector(rownames(min_n_mth_lcl_ccy_returns)), Date=as.vector(min_n_mth_lcl_ccy_ret_dates), Min_Lcl_Ccy_Ret=min_n_mth_lcl_ccy_returns[,1], Corr_Glbl_Pf_USD_Ret=glbl_pf_return_n_mth_contemporaneous[,1])
}
myListForLogs <- map(nMths, ~logLclGlblDataPoints(.x, listOfDataSets = listOfDataFrames))

postMeltingData <- as.data.frame(do.call(rbind.data.frame,myListForLogs))

plotScatterDiagrams <- function(deshMudra, dataSet){
  plotTitle <- sprintf("Worst %s vs. corr. glbl. pf. ret.",deshMudra)
  dataSet %>% filter(Country_Ccy==deshMudra) %>% ggplot(aes(x=Min_Lcl_Ccy_Ret, y=Corr_Glbl_Pf_USD_Ret, color=nMonths)) + geom_point() + scale_color_gradientn(colours = rainbow(5)) + geom_abline(slope=1, intercept=0, color="gray") +xlim(-1,1) +ylim(-1,1) + labs(title=plotTitle, subtitle="1-120 calendar months, Dec-99 to Dec-21", x="Worst domestic equity index return in local currency", y="Corr. glbl. pf. USD ret.")
}
countryCcyNames <- unique(postMeltingData$Country_Ccy)
myPlots <- map(countryCcyNames, ~plotScatterDiagrams(.x, dataSet = postMeltingData))

# XY_scatter_plot_01 <- as.grob(myPlots[1])
# XY_scatter_plot_01 <- as.grob(myPlots[[1]])
# XY_scatter_plot_02 <- as.grob(myPlots[[2]])
# XY_scatter_plot_03 <- as.grob(myPlots[[3]])
# XY_scatter_plot_04 <- as.grob(myPlots[[4]])
# XY_scatter_plot_05 <- as.grob(myPlots[[5]])
# XY_scatter_plot_06 <- as.grob(myPlots[[6]])
# XY_scatter_plot_07 <- as.grob(myPlots[[7]])
# XY_scatter_plot_08 <- as.grob(myPlots[[8]])
# XY_scatter_plot_09 <- as.grob(myPlots[[9]])
# XY_scatter_plot_10 <- as.grob(myPlots[[10]])
# XY_scatter_plot_11 <- as.grob(myPlots[[11]])
# XY_scatter_plot_12 <- as.grob(myPlots[[12]])
# XY_scatter_plot_13 <- as.grob(myPlots[[13]])
# XY_scatter_plot_14 <- as.grob(myPlots[[14]])
# XY_scatter_plot_15 <- as.grob(myPlots[[15]])
# XY_scatter_plot_16 <- as.grob(myPlots[[16]])
# XY_scatter_plot_17 <- as.grob(myPlots[[17]])
# XY_scatter_plot_18 <- as.grob(myPlots[[18]])
# XY_grid_01 <- grid.arrange(XY_scatter_plot_01, XY_scatter_plot_12, XY_scatter_plot_04, XY_scatter_plot_03, XY_scatter_plot_09, XY_scatter_plot_10, XY_scatter_plot_08, XY_scatter_plot_15, XY_scatter_plot_13, nrow=3)
# ggsave("XY_grid_01.png", XY_grid_01, width=14, height=8.50, units="in", scale=1)
# XY_grid_02 <- grid.arrange(XY_scatter_plot_02, XY_scatter_plot_16, XY_scatter_plot_17, XY_scatter_plot_18, XY_scatter_plot_11, XY_scatter_plot_05, XY_scatter_plot_14, XY_scatter_plot_07, XY_scatter_plot_06, nrow=3)
# ggsave("XY_grid_02.png", XY_grid_02, width=14, height=8.50, units="in", scale=1)
