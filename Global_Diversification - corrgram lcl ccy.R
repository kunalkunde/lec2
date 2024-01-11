### This R script has been created to make a corrgram
#   of global equity indices' monthly lcl-ccy returns   
library(ggalt)
library(ggpmisc)
library(grid)
library(gridExtra)
library(corrgram)
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
Equity_Conversion_Factors <- data.frame(Equity_Index_Dates_n_Values$Date,t(t(Equity_Index_Values)/t(Equity_Index_Values)[,1])) 
colnames(Equity_Conversion_Factors)[1] <- "Date"
mthLclCcyRetDataForCorrgram <- Equity_Conversion_Factors[2:265,2:19]-1
colnames(mthLclCcyRetDataForCorrgram) <- c("Aus","Jpn","Chn","Can","Sui","USA","UK","HK","Fra","Ger","S Kor","Bra","Ire","Twn","Ind","Ned","Rus","SA")
mthLclCcyRetDataForCorrgram <- mthLclCcyRetDataForCorrgram[,order(colnames(mthLclCcyRetDataForCorrgram))]

correlationMatrix <- cor(mthLclCcyRetDataForCorrgram)
correlationMatrix = format(round(correlationMatrix[,1:18],2),nsmall=2)
#GrObForMyCorrelationMatrix <- tableGrob(correlationMatrix, rows = colnames(mthLclCcyRetDataForCorrgram), cols = colnames(mthLclCcyRetDataForCorrgram))
#ggsave("CorrMatrix_eq_ret_per_lcl_ccy.png", GrObForMyCorrelationMatrix, width=14, height=8.50, units="in", scale=1)
write.table(correlationMatrix, file = "correlation_matrix_eq_ret_per_lcl_ccy.txt", sep ="\t", row.names = TRUE, col.names = TRUE )

corrgram(mthLclCcyRetDataForCorrgram, order=FALSE, lower.panel=panel.pts, upper.panel=panel.pie, text.panel=panel.txt, main="Corrgram of monthly returns in lcl ccy\n31-Dec-1999 to 15-Dec-2021")
# save/export this as a 1200 X 800 png image
