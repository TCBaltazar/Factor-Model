library(devtools, pacman)
pacman::p_load(covFactorModel, FinCovRegularization, xts, quantmod, tidyverse,
               kableExtra, lubridate, tbl2xts, FinCovRegularization, corrplot, tibbletime)

# Using covFactorModel EXAMPLE

# covFactorModel::factorModel() -> builds factor model for given data..
#       decomposes the asset returns into a factor component and a residual component

# covFactorModel::covFactorModel() -> covariance matrix estimation via factor models
# covFactorModel::getSectorInfo()


dat <- m.excess.c10sp9003

assets <- dat[, 1:10]
factor <- dat[, 11]

T <- nrow(assets)

# Converting data to xts object

assets_xts <- as.xts(assets, order.by=as.Date("1995-03-15")+1:T)
factor_xts <- as.xts(factor, order.by=as.Date("1995-03-15")+1:T)

# sector information for BARRA Industry factor model
beta <- matrix(0, 10, 3)
dimnames(beta) <- list(colnames(assets), c("Drug", "Auto", "Oil"))
beta[c("ABT", "LLY", "MRK", "PFE"), "Drug"] <- 1
beta[c("F", "GM"), "Auto"] <- 1
beta[c("BP", "CVX", "RD", "XOM"), "Oil"] <- 1
sector_info <- c(rep(1, 4),
                 rep(2, 2),
                 rep(3, 4))

# compare cov by macroeconomic factor model
mcov_MACRO <- covFactorModel(assets_xts, type="Macro", econ_fact=factor_xts)
cov_macro <- MacroFactor.Cov(assets, factor)
norm(cov_macro - mcov_MACRO, "F")



# compare cov by BARRA Industry factor model
mcov_BARRA <- covFactorModel(assets_xts, type = "Barra", stock_sector_info = sector_info)
cov_BARRA <- FundamentalFactor.Cov(assets, exposure = beta, method = "OLS")
norm(cov_BARRA - mcov_BARRA, "F")


# compare cov by statistical factor model
mcov_STAT <- covFactorModel(assets_xts, type = "Stat-PCA", K = 3)
cov_STAT <- StatFactor.Cov(assets, 3)
norm(cov_STAT - mcov_STAT, "F")


# cov2cor() -> converts covariance matrix to correlation matrix

#   usage of covFactorModel::covFactorModel()



# Impute missing returns function

impute_missing_returns <- function(return_mat, impute_returns_method = "NONE", Seed = 1234){
    # Make sure we have a date column called date:
    if( !"date" %in% colnames(return_mat) ) stop("No 'date' column provided in return_mat. Try again please.")

    # Note my use of 'any' below...
    # Also note that I 'return' return_mat - which stops the function and returns return_mat.
    if( impute_returns_method %in% c("NONE", "None", "none") ) {
        if( any(is.na(return_mat)) ) warning("There are missing values in the return matrix.. Consider maybe using impute_returns_method = 'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
        return(return_mat)
    }


    if( impute_returns_method  == "Average") {

        return_mat <-
            return_mat %>% gather(Stocks, Returns, -date) %>%
            group_by(date) %>%
            mutate(Avg = mean(Returns, na.rm=T)) %>%
            mutate(Avg = coalesce(Avg, 0)) %>% # date with no returns - set avg to zero
            ungroup() %>%
            mutate(Returns = coalesce(Returns, Avg)) %>% select(-Avg) %>% spread(Stocks, Returns)

        # That is just so much easier when tidy right? See how I gathered and spread again to give back a wide df?

    } else

        if( impute_returns_method  == "Drawn_Distribution_Own") {

            set.seed(Seed)
            N <- nrow(return_mat)
            return_mat <-

                left_join(return_mat %>% gather(Stocks, Returns, -date),
                          return_mat %>% gather(Stocks, Returns, -date) %>% group_by(Stocks) %>%
                              do(Dens = density(.$Returns, na.rm=T)) %>%
                              ungroup() %>% group_by(Stocks) %>% # done to avoid warning.
                              do(Random_Draws = sample(.$Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y)),
                          by = "Stocks"
                ) %>%  group_by(Stocks) %>% mutate(Row = row_number()) %>% mutate(Returns = coalesce(Returns, Random_Draws[[1]][Row])) %>%
                select(-Random_Draws, -Row) %>% ungroup() %>% spread(Stocks, Returns)

        } else

            if( impute_returns_method  == "Drawn_Distribution_Collective") {

                set.seed(Seed)
                NAll <- nrow(return_mat %>% gather(Stocks, Returns, -date))

                return_mat <-
                    bind_cols(
                        return_mat %>% gather(Stocks, Returns, -date),
                        return_mat %>% gather(Stocks, Returns, -date) %>%
                            do(Dens = density(.$Returns, na.rm=T)) %>%
                            do(Random_Draws = sample(.$Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y)) %>% unnest(Random_Draws)
                    ) %>%
                    mutate(Returns = coalesce(Returns, Random_Draws)) %>% select(-Random_Draws) %>% spread(Stocks, Returns)

            } else

                if( impute_returns_method  == "Zero") {
                    warning("This is probably not the best idea but who am I to judge....")
                    return_mat[is.na(return_mat)] <- 0

                } else
                    stop("Please provide a valid impute_returns_method method. Options include:\n'Average', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")
}

# Now we will use this function as follows (after saving and sourcing it of course....):
# Note my seed is the year, day hour and minute - so unless you do this multiple times a minute, it will always differ.

options(scipen = 999)

# DATA

bonds_10 <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/bonds_10y.rds")
comms <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/comms.rds")
IV <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/IV.rds")
SA_Bonds <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/SA_Bonds.rds")
T40 <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/T40.rds")
usdzar <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/usdzar.rds")
ZA_Infl <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/ZA_Infl.rds")

# Transformations

US_10Yr <- bonds_10 %>%
    filter(Name=="US_10Yr") %>%
    spread(Name, Bond_10Yr) # daily -> 1990-01-01 : 2021-10-29

comms <- comms %>%
    filter(Name=="Bcom_Index") %>%
    spread(Name, Price) # daily -> 1990-01-01 : 2021-10-29

VIX <- IV %>%
    filter(Name=="VIX") %>%
    spread(Name, Price) # daily -> 1990-01-02 : 2021-10-29

SA_10Yr <- SA_Bonds %>%
    select(date, ZA_10Yr) %>%
    rename(SA_10Yr=ZA_10Yr) # daily -> 1992-12-06 : 2021-10-29


T40 <- T40 %>%
    filter(Index_Name=="Large_Caps") %>%    # only considering large cap stocks
    select(date, Tickers, Return)

T40$Tickers <- gsub(" SJ Equity", "", T40$Tickers)

T40 <- T40 %>% spread(Tickers, Return) # daily -> 2008-01-02 : 2021-10-29


usdzar$Name <- gsub("SouthAfrica_Cncy", "USDZAR", usdzar$Name)
usdzar <- usdzar %>% spread(Name, Price) # daily -> 1990-01-01 : 2021-10-29

ZA_Infl <- ZA_Infl %>% spread(Name, Price) # monthly -> 1990-01-01 : 2021-10-31

rm(bonds_10, IV, SA_Bonds)  # removing old data


# T40 %>% select(where(~!all(is.na(.)))) %>% names() remove columns with only NA

# T40 %>% select(where(~!any(is.na(.)))) %>% names() Remove columns with any NA

T40 <- T40[, which(colMeans(!is.na(T40)) > 0.60)] # remove stocks where >60% of observations are NA



T40_clean <-
    impute_missing_returns(T40, impute_returns_method = "Drawn_Distribution_Own", Seed = as.numeric(format( Sys.time(), "%Y%d%H%M")))


dat <- read.csv("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/DAT.csv")
######################################## IMPORTANT ############################################################################
new_dat <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/alsi.rds")

newdat_Industrial <- new_dat %>%
    filter(Superindustryname=="Industrials") %>%
    select(date, Tickers, Return)
newdat_Industrial$Tickers <- gsub(" SJ Equity", "", newdat_Industrial$Tickers)

newdat_Industrial <- newdat_Industrial %>% pivot_wider(names_from = Tickers, values_from = Return)

newdat_Resources <- new_dat %>%
    filter(Superindustryname=="Resources") %>%
    select(date, Tickers, Return)
newdat_Resources$Tickers <- gsub(" SJ Equity", "", newdat_Resources$Tickers)

newdat_Resources <- newdat_Resources %>% pivot_wider(names_from = Tickers, values_from = Return)

newdat_Financials <- new_dat %>%
    filter(Superindustryname=="Financials") %>%
    select(date, Tickers, Return)
newdat_Financials$Tickers <- gsub(" SJ Equity", "", newdat_Financials$Tickers)

newdat_Financials <- newdat_Financials %>% pivot_wider(names_from = Tickers, values_from = Return)



newdat %>% count(., Tickers, sort=TRUE)

newdat1 <- newdat[, which(colMeans(!is.na(newdat)) > 0.60)]



# consolidated data

est_dat <- left_join(T40_clean,
                     US_10Yr %>% filter(date>ymd("2008-01-01")),
                     by="date") %>%
    left_join(.,
              SA_10Yr %>% filter(date>ymd("2008-01-01")),
              by="date") %>%
    left_join(.,
              usdzar %>% filter(date>ymd("2008-01-01")),
              by="date") %>%
    left_join(.,
              VIX %>% filter(date>ymd("2008-01-01")),
              by="date") %>%
    left_join(.,
              comms %>% filter(date>ymd("2008-01-01")),
              by="date")


data.frame(dateconverter(as.Date("2008-01-02"), as.Date("2021-07-01"),"weekdayEOQ"))


write.csv(dat %>%
              pivot_longer(cols = starts_with("X"), names_to="date", values_drop_na = FALSE) %>%
              pivot_wider(names_from="Indicator.Name") %>%
              mutate_at("date", str_replace, "X", "") %>%
              arrange(date) %>% mutate(date=as.yearqtr(date, format = "%YQ%q")) %>%
              mutate(date=as.Date(date)),
          file="/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/dat1.csv")


dat1 <- read.csv("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/dat1.csv")

dat1 <- as_tibble(dat1) %>% mutate(date=as.Date(dmy(date))) %>% tbl_xts()

write.csv(est_dat, file="/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/full.csv")


est_dat1 <- read.csv("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/full.csv")

est_dat1 <- est_dat1 %>%
    as_tibble(.) %>% mutate(date=as.yearqtr(date, format = "%Y Q%q")) %>%
    select(-SA_10Yr)


dat <- dat %>%
    pivot_longer(cols = starts_with("X"), names_to="date", values_drop_na = FALSE) %>%
    pivot_wider(names_from="Indicator.Name") %>%
    mutate_at("date", str_replace, "X", "") %>%
    arrange(date) %>% mutate(date=as.yearqtr(date, format = "%Y Q%q")) %>%
    filter(date>first(date))


full_dat <- left_join(est_dat1,
                      dat,
                      by="date")


new_dat <- read_rds("/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/alsi.rds")

asnew_dat %>% new_dat[, which(colMeans(!is.na(new_dat$Return)) > 0.60)] %>%
    colnames(.)






est_dat %>% colnames(.)
est_dat %>% ncol(.)

assets_xts <- est_dat[, -c(82:86)] %>% tbl_xts()
factors_xts <- est_dat[, -c(2:81)] %>% tbl_xts()

# Factor Model

FM <- covFactorModel::factorModel(assets_xts, type="Macro", econ_fact = factors_xts, rtn_Sigma = TRUE)

par(mfrow = c(1, 2))
barplot(FM$alpha, horiz = TRUE,
        main = "alpha", col = "red", cex.names = 0.75, las = 1)
barplot(t(FM$beta), horiz = TRUE,
        main = "beta", col = "blue", cex.names = 0.75, las = 1)

cbind(alpha = FM$alpha, beta = FM$beta)

corrplot(cov2cor(FM$Sigma),
         main = "Covariance matrix of log-returns from 1-factor model")


# Covariance Matrix Estimation using Factor Model


Macro_FM <- covFactorModel(assets_xts, type = "Macro", econ_fact = factors_xts, rtn_Sigma)

Macro_FM$



# compare cov by macroeconomic factor model
mcov_MACRO <- covFactorModel(assets_xts, type="Macro", econ_fact=factors_xts)
cov_macro <- MacroFactor.Cov(assets, factor)
norm(cov_macro - mcov_MACRO, "F")

# cov2cor() -> converts covariance matrix to correlation matrix



M <- full_dat%>%dplyr::select(-date)
M1 <- cor(M)
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
}
p.mat <- cor.mtest(M)

corrplot(M1, type="lower", order="hclust")

corrplot(M1, type="lower", order="hclust",  p.mat = p.mat, sig.level = 0.01, insig = "blank")

write.csv(full_dat,
          file="/Users/tiagob/Documents/Masters 2021/Second Semester/Financial Econometrics/Project/Factor Modeling/Factor_Model/Written/Factor Model/data/finaldat.csv")



















