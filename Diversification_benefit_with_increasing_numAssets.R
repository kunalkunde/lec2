### As per the advice in a StackOverflow posting, at the following link:
#   https://stackoverflow.com/questions/63559509/r-evaluate-function-for-multiple-and-differently-changing-arguments
### Unfortunately, the outer(X,Y,function) call outlined in the thread at the URL below does not seem to work (when I attempt it)
#   https://stackoverflow.com/questions/33143282/elegantly-evaluating-a-function-of-two-variables-for-two-value-vectors-in-r

library(tidyverse)

num_assets_vector <- (1:40) # map does not seem to work when these values are passed in a data frame, hence using a vector
corr_vector <- 0.1*(0:9) # map does not seem to work when these values are passed in a data frame, hence using a vector  
secVols_vector <- 0.01*(9:18) # map does not seem to work when these values are passed in a data frame, hence using a vector

eq_wt_pf_vol <- function(nSecurities, pairwiseCorrelationOfSecurities=0.4,secVol=0.1)
{ 
  pf_variance <- 0
  for(i in 1:nSecurities)
  {
    for(j in 1:nSecurities) 
    { 
      if(i==j) {pf_variance <- pf_variance + (1/nSecurities)*(1/nSecurities)*secVol*secVol}
      if(i!=j) {pf_variance <- pf_variance + (1/nSecurities)*(1/nSecurities)*pairwiseCorrelationOfSecurities*secVol*secVol}
    }
  }
  sqrt(pf_variance) 
}

vols_000pctCorr <- num_assets_vector %>% map(~eq_wt_pf_vol(.x,0,0.1))
vols_020pctCorr <- num_assets_vector %>% map(~eq_wt_pf_vol(.x,0.2,0.1))
vols_040pctCorr <- num_assets_vector %>% map(~eq_wt_pf_vol(.x,0.4,0.1))
vols_060pctCorr <- num_assets_vector %>% map(~eq_wt_pf_vol(.x,0.6,0.1))
vols_080pctCorr <- num_assets_vector %>% map(~eq_wt_pf_vol(.x,0.8,0.1))
vols_100pctCorr <- num_assets_vector %>% map(~eq_wt_pf_vol(.x,1.0,0.1))
eqWtPfVolNosContainer <- as.data.frame(rbind(vols_000pctCorr, vols_020pctCorr, vols_040pctCorr, vols_060pctCorr, vols_080pctCorr, vols_100pctCorr))
colnames(eqWtPfVolNosContainer) <- paste0("nAssets_",1:40)
eqWtPfVolNos_df <- as.data.frame(do.call(rbind.data.frame, eqWtPfVolNosContainer))

ggplot(eqWtPfVolNos_df, aes(x=1:40))+geom_line(aes(y=vols_000pctCorr,colour="0%"))+geom_line(aes(y=vols_020pctCorr,colour="20%"))+geom_line(aes(y=vols_040pctCorr,colour="40%"))+geom_line(aes(y=vols_060pctCorr,colour="60%"))+geom_line(aes(y=vols_080pctCorr,colour="80%"))+geom_line(aes(y=vols_100pctCorr,colour="100%"))+scale_color_manual(name = "Pairwise asset\n correlation", values = c("0%" = "orange", "20%" = "red","40%" = "green", "60%" = "brown","80%" = "blue", "100%" = "black"))+labs(title="Portfolio volatility by number of assets", subtitle="(equal-weight portfolio, with each asset having 10% p.a. vol)", x="Number of assets in the portfolio", y="Volatility of equal-weighted portfolio")
