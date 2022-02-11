# Short Note

library(pacman)
pacman::p_load(tidyverse, tbl2xts, covFactorModel)
dat <- read.csv("/finaldat.csv")


dat %>% colnames(.)
dat %>% ncol(.)

assets_xts <- dat[, -c(36:43)] %>% tbl_xts() #subset of asset returns
factors_xts <- dat[, -c(2:35)] %>% tbl_xts() #subset of macro factors

FM <- covFactorModel::factorModel(assets_xts, type="Macro", econ_fact = factors_xts, rtn_Sigma = TRUE)

Macro_FM <- covFactorModel(assets_xts, type = "Macro", econ_fact = factors_xts, rtn_Sigma)

# when I first ran the model excluding RealGDP, Investment, CPI, and the MM rate and daily data, there would be no error
# now the message is that the 'system is computationally singular'