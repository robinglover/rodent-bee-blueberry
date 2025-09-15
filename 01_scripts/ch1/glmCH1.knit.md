---
title: "glmCH1"
output: pdf_document
---



# Load and clean data


``` r
#load packages
library(tidyverse)
```

```
## -- Attaching core tidyverse packages ------------------------ tidyverse 2.0.0 --
## v dplyr     1.1.4     v readr     2.1.5
## v forcats   1.0.0     v stringr   1.5.1
## v ggplot2   3.5.2     v tibble    3.3.0
## v lubridate 1.9.4     v tidyr     1.3.1
## v purrr     1.0.4     
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
## i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(ggplot2)
library(DHARMa)
```

```
## This is DHARMa 0.4.7. For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa')
```

``` r
library(glmmTMB)

#read in data
data <- read.csv("06_cleandata/combined_plot_data.csv")

#check that variables are in appropriate format
str(data)
```

```
## 'data.frame':	210 obs. of  34 variables:
##  $ site_id                 : chr  "bates_57b" "bates_57b" "bates_57b" "bates_57b" ...
##  $ round                   : chr  "2024_r1" "2024_r1" "2024_r1" "2024_r2" ...
##  $ plot_id                 : chr  "p1" "p2" "d" "p1" ...
##  $ bee_date                : chr  "2024-04-15" "2024-04-15" "2024-04-15" "2024-06-06" ...
##  $ julian.date             : int  106 106 106 158 158 158 173 173 173 101 ...
##  $ bee_season_type         : chr  "nestsearching" "nestsearching" "nestsearching" "worker" ...
##  $ area                    : chr  "delta_1" "delta_1" "delta_1" "delta_1" ...
##  $ cover_type              : chr  "grass" "grass" "grass" "grass" ...
##  $ bare_perc               : num  2 1.42 0 11.92 20.5 ...
##  $ grass_perc              : num  46.8 52.2 90.4 52.7 56.8 ...
##  $ forb_perc               : num  9.33 7.92 8 16.75 8.12 ...
##  $ grass_forb_perc         : num  56.2 60.2 98.4 69.4 64.9 ...
##  $ grass_h                 : num  17.9 19.5 28.8 25.3 25.7 ...
##  $ forb_h                  : num  9.83 7.42 9.58 26.17 17.42 ...
##  $ floral_perc             : num  0.375 1.0417 0.125 0.0417 0 ...
##  $ bombus_floral_perc      : num  0.375 1.0417 0.0833 0.0417 0 ...
##  $ grass_forb_floral_perc  : num  56.5 61.2 98.5 69.5 64.9 ...
##  $ flood_perc              : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ dry_root_perc           : num  25.42 37.17 7.08 16.25 5.42 ...
##  $ bryo_perc               : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ bare_flood_dry_bryo_perc: num  27.42 38.58 7.08 28.17 25.92 ...
##  $ bare_dry_bryo_perc      : num  27.42 38.58 7.08 28.17 25.92 ...
##  $ floral_richness         : int  1 1 3 2 3 14 2 2 12 2 ...
##  $ bombus_floral_richness  : int  1 1 2 2 3 11 2 2 10 2 ...
##  $ floral_abun             : num  3 3 3.32 2.01 2.01 ...
##  $ bombus_floral_abun      : num  3 3 3.04 2.01 2.01 ...
##  $ nest_searching_queens   : int  3 0 4 2 0 0 0 0 0 0 ...
##  $ workers                 : int  1 0 0 0 1 2 0 0 0 0 ...
##  $ forage_vaco             : int  1 2 0 2 4 0 0 0 0 3 ...
##  $ num_burrows             : int  41 120 93 41 120 93 41 120 93 41 ...
##  $ num_inactive_burrows    : int  10 64 0 10 64 0 10 64 0 10 ...
##  $ num_active_dm           : int  5 6 0 5 6 0 5 6 0 5 ...
##  $ num_dm_size             : int  3 3 0 3 3 0 3 3 0 3 ...
##  $ num_inactive_dm         : int  1 2 0 1 2 0 1 2 0 1 ...
```

``` r
#change character columns to factors
surveys <- data %>%
   mutate(
     site_id = as.factor(site_id),
     round = as.factor(round),
     plot_id = as.factor(plot_id),
     bee_date = as.Date(bee_date),
     bee_season_type = as.factor(bee_season_type),
     area = as.factor(area))
```

Scale continuous predictors


``` r
surveys$julian.date_scaled <- as.numeric(scale(surveys$julian.date))
surveys$bare_perc_scaled <- as.numeric(scale(surveys$bare_perc))
surveys$grass_perc_scaled <- as.numeric(scale(surveys$grass_perc))
surveys$forb_perc_scaled <- as.numeric(scale(surveys$forb_perc))
surveys$grass_forb_perc_scaled <- as.numeric(scale(surveys$grass_forb_perc))
surveys$grass_h_scaled <- as.numeric(scale(surveys$grass_h))
surveys$forb_h_scaled <- as.numeric(scale(surveys$forb_h))
surveys$floral_perc_scaled <- as.numeric(scale(surveys$floral_perc))
surveys$grass_forb_floral_perc_scaled <- as.numeric(scale(surveys$grass_forb_floral_perc))
surveys$bombus_floral_perc_scaled <- as.numeric(scale(surveys$bombus_floral_perc))
surveys$flood_perc_scaled <- as.numeric(scale(surveys$flood_perc))
surveys$dry_root_perc_scaled <- as.numeric(scale(surveys$dry_root_perc))
surveys$bryo_perc_scaled <- as.numeric(scale(surveys$bryo_perc))
surveys$bare_flood_dry_bryo_perc_scaled <- as.numeric(scale(surveys$bare_flood_dry_bryo_perc))
surveys$floral_richness_scaled <- as.numeric(scale(surveys$floral_richness))
surveys$bombus_floral_richness_scaled <- as.numeric(scale(surveys$bombus_floral_richness))
surveys$floral_abun_scaled <- as.numeric(scale(surveys$floral_abun))
surveys$bombus_floral_abun_scaled <- as.numeric(scale(surveys$bombus_floral_abun))
surveys$nest_searching_queens_scaled <- as.numeric(scale(surveys$nest_searching_queens))
surveys$workers_scaled <- as.numeric(scale(surveys$workers))
surveys$forage_vaco_scaled <- as.numeric(scale(surveys$forage_vaco))
surveys$num_burrows_scaled <- as.numeric(scale(surveys$num_burrows))
surveys$num_inactive_burrows_scaled <- as.numeric(scale(surveys$num_inactive_burrows))
surveys$num_active_dm_scaled <- as.numeric(scale(surveys$num_active_dm))
surveys$num_dm_size_scaled <- as.numeric(scale(surveys$num_dm_size))
surveys$num_inactive_dm_scaled <- as.numeric(scale(surveys$num_inactive_dm))
```

Filter to only include field data


``` r
field_data <- surveys %>%
  filter(plot_id != "d")
```

# GLM #1: Number of nest-searching queens

Plotting a zero-inflated poisson model


``` r
nsq_lm_zeroPois <- glmmTMB(nest_searching_queens ~ grass_forb_perc_scaled + 
                    bombus_floral_abun_scaled + 
                    num_burrows_scaled + 
                     num_inactive_burrows_scaled + 
                    bee_season_type, 
                  ziformula = ~1,  # zero-inflation model
                  family = poisson, #poisson
                  data = field_data)

summary(nsq_lm_zeroPois)
```

```
##  Family: poisson  ( log )
## Formula:          
## nest_searching_queens ~ grass_forb_perc_scaled + bombus_floral_abun_scaled +  
##     num_burrows_scaled + num_inactive_burrows_scaled + bee_season_type
## Zero inflation:                         ~1
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##     151.3     171.9     -68.7     137.3       133 
## 
## 
## Conditional model:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   0.2772     0.4464   0.621  0.53462    
## grass_forb_perc_scaled        0.2930     0.2493   1.175  0.23980    
## bombus_floral_abun_scaled     0.2277     0.2092   1.088  0.27652    
## num_burrows_scaled            2.3470     1.1074   2.119  0.03405 *  
## num_inactive_burrows_scaled  -1.9770     0.9995  -1.978  0.04793 *  
## bee_season_typeworker        -2.2642     0.6247  -3.624  0.00029 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -0.5993     0.8062  -0.743    0.457
```

Diagnostics


``` r
#simulate residuals

residuals_nsq_zeroPois <- simulateResiduals(fittedModel = nsq_lm_zeroPois, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

``` r
##no evidence of overdispersion or significant outliers
```

Plotting a binary model to predict if #NSQ \> 0


``` r
#create binary variable for nsq presence
field_data$nsq_binary <- ifelse(field_data$nest_searching_queens > 0, 1, 0)

nsq_lm_binary <- glmmTMB(nsq_binary ~ grass_forb_perc_scaled + 
                    bombus_floral_abun_scaled + 
                    num_burrows_scaled + 
                    num_inactive_burrows_scaled + 
                    bee_season_type, 
                  family = binomial, 
                  data = field_data)

summary(nsq_lm_binary)
```

```
##  Family: binomial  ( logit )
## Formula:          
## nsq_binary ~ grass_forb_perc_scaled + bombus_floral_abun_scaled +  
##     num_burrows_scaled + num_inactive_burrows_scaled + bee_season_type
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##      97.8     115.4     -42.9      85.8       134 
## 
## 
## Conditional model:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -0.3569     0.6149  -0.580 0.561590    
## grass_forb_perc_scaled        1.0014     0.3596   2.785 0.005357 ** 
## bombus_floral_abun_scaled     0.4924     0.2880   1.710 0.087303 .  
## num_burrows_scaled            1.3950     1.7315   0.806 0.420430    
## num_inactive_burrows_scaled  -1.6596     1.4770  -1.124 0.261167    
## bee_season_typeworker        -2.6963     0.7890  -3.417 0.000633 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

# GLM #2: Number of workers

Model selection between poisson, negative binomial and zero-inflated models.

First, plot it as a poisson model.


``` r
worker_lm_pois <- glmmTMB(workers ~ nest_searching_queens_scaled +
                       bombus_floral_abun_scaled + 
                       num_burrows_scaled + 
                       num_inactive_burrows_scaled +
                       bee_season_type, 
                     family = poisson,
                     data = field_data
                       )

summary(worker_lm_pois)
```

```
##  Family: poisson  ( log )
## Formula:          
## workers ~ nest_searching_queens_scaled + bombus_floral_abun_scaled +  
##     num_burrows_scaled + num_inactive_burrows_scaled + bee_season_type
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##     200.5     218.2     -94.3     188.5       134 
## 
## 
## Conditional model:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -2.3978     0.5046  -4.752 2.02e-06 ***
## nest_searching_queens_scaled  -0.1122     0.4338  -0.259 0.795887    
## bombus_floral_abun_scaled      0.4441     0.1938   2.291 0.021954 *  
## num_burrows_scaled             0.6413     0.8349   0.768 0.442429    
## num_inactive_burrows_scaled   -0.3711     0.6694  -0.554 0.579283    
## bee_season_typeworker          1.5872     0.4811   3.299 0.000971 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Run diagnostic tests


``` r
#simulate residuals

residuals_workers_pois <- simulateResiduals(fittedModel = worker_lm_pois, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 

``` r
##residual plots show that there is significant overdispersion --> i should plot the negative binomial to account for this
```

Plot negative binomial


``` r
worker_lm_negbin <- glmmTMB(workers ~ nest_searching_queens_scaled +
                       bombus_floral_abun_scaled + 
                       num_burrows_scaled + 
                       num_inactive_burrows_scaled +
                       bee_season_type, 
                     family = nbinom1,
                     data = field_data
                       )
```

Diagnostics


``` r
#simulate residuals

residuals_workers_negbin <- simulateResiduals(fittedModel = worker_lm_negbin, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

``` r
##residual plots show that there is no overdispersion and no excessive outliers. this shows that the negative binomial is a much better fit
```

Test zero-inflation


``` r
testZeroInflation(residuals_workers_negbin)
```

![](glmCH1_files/figure-latex/unnamed-chunk-11-1.pdf)<!-- --> 

```
## 
## 	DHARMa zero-inflation test via comparison to expected zeros with
## 	simulation under H0 = fitted model
## 
## data:  simulationOutput
## ratioObsSim = 1.0003, p-value = 1
## alternative hypothesis: two.sided
```

``` r
##p-value = 1. No evidence of zero-inflation. Do not fit zero-inflated model, leave as negative binomial.
```

Look at results of final model


``` r
summary(worker_lm_negbin)
```

```
##  Family: nbinom1  ( log )
## Formula:          
## workers ~ nest_searching_queens_scaled + bombus_floral_abun_scaled +  
##     num_burrows_scaled + num_inactive_burrows_scaled + bee_season_type
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##     163.0     183.6     -74.5     149.0       133 
## 
## 
## Dispersion parameter for nbinom1 family (): 1.94 
## 
## Conditional model:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -2.04281    0.61003  -3.349 0.000812 ***
## nest_searching_queens_scaled  0.05988    0.44176   0.136 0.892178    
## bombus_floral_abun_scaled     0.49887    0.27228   1.832 0.066917 .  
## num_burrows_scaled            0.09062    1.08744   0.083 0.933589    
## num_inactive_burrows_scaled   0.09220    0.81302   0.113 0.909706    
## bee_season_typeworker         0.92043    0.54647   1.684 0.092118 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

# GLM #3: Number of burrows

First, plot poisson


``` r
burrows_lm_pois <- glmmTMB(num_burrows ~ grass_forb_perc_scaled + 
                    julian.date_scaled, 
                  family = poisson, #poisson
                  data = field_data)
```

Diagnostics


``` r
#simulate residuals

residuals_burrows_pois <- simulateResiduals(fittedModel = burrows_lm_pois, plot = TRUE)
```

```
## Warning in newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,
## : Fitting terminated with step failure - check results carefully
```

![](glmCH1_files/figure-latex/unnamed-chunk-14-1.pdf)<!-- --> 

``` r
##residual plots show that there is overdispersion and  excessive outliers. Plot negative binomial
```

Plot negative binomial to see if that fixes the overdispersion.


``` r
burrows_lm_negbin <- glmmTMB(num_burrows ~ grass_forb_perc_scaled + 
                    julian.date_scaled, 
                  family = nbinom1, 
                  data = field_data)
```

Diagnostics


``` r
#simulate residuals

residuals_burrows_negbin <- simulateResiduals(fittedModel = burrows_lm_negbin, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-16-1.pdf)<!-- --> 

``` r
##residual plots show that there is no overdispersion or excessive outliers. Now check zero-inflation.
```

Test zero-inflation


``` r
testZeroInflation(residuals_burrows_negbin)
```

![](glmCH1_files/figure-latex/unnamed-chunk-17-1.pdf)<!-- --> 

```
## 
## 	DHARMa zero-inflation test via comparison to expected zeros with
## 	simulation under H0 = fitted model
## 
## data:  simulationOutput
## ratioObsSim = 0.81235, p-value = 0.256
## alternative hypothesis: two.sided
```

``` r
##p-value = 0.256. No strong evidence of zero-inflation. I'll try the zero-inflated model to see if it helps. 
```

Plot zero-inflated negative binomial and recheck diagnostics.


``` r
burrows_lm_zero_negbin <- glmmTMB(num_burrows ~ grass_forb_perc_scaled + 
                    julian.date_scaled, 
                    ziformula = ~1,  # zero-inflation model
                  family = nbinom1, 
                  data = field_data)
```

Diagnostics


``` r
#simulate residuals

residuals_burrows_zero_negbin <- simulateResiduals(fittedModel = burrows_lm_zero_negbin, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-19-1.pdf)<!-- --> 

``` r
##residual plots does not look significantly better than the non-zero-inflated model.
```

Compare AIC values between the 2 models


``` r
summary(burrows_lm_negbin)
```

```
##  Family: nbinom1  ( log )
## Formula:          num_burrows ~ grass_forb_perc_scaled + julian.date_scaled
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##    1052.0    1063.7    -522.0    1044.0       136 
## 
## 
## Dispersion parameter for nbinom1 family (): 48.9 
## 
## Conditional model:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.11709    0.12941  24.086  < 2e-16 ***
## grass_forb_perc_scaled  0.43025    0.10936   3.934 8.35e-05 ***
## julian.date_scaled      0.08301    0.08634   0.961    0.336    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
summary(burrows_lm_zero_negbin)
```

```
##  Family: nbinom1  ( log )
## Formula:          num_burrows ~ grass_forb_perc_scaled + julian.date_scaled
## Zero inflation:               ~1
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##    1054.0    1068.7    -522.0    1044.0       135 
## 
## 
## Dispersion parameter for nbinom1 family (): 48.9 
## 
## Conditional model:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.11709    0.12941  24.086  < 2e-16 ***
## grass_forb_perc_scaled  0.43026    0.10936   3.934 8.35e-05 ***
## julian.date_scaled      0.08301    0.08634   0.961    0.336    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)   -20.08    4667.00  -0.004    0.997
```

``` r
##AIC and BIC values are slightly lover for negative binomial and there was no strong evidence for zero-inflation. Therefore, I think the negative binomal model is the best. 
```

Results for the final model


``` r
summary(burrows_lm_negbin)
```

```
##  Family: nbinom1  ( log )
## Formula:          num_burrows ~ grass_forb_perc_scaled + julian.date_scaled
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##    1052.0    1063.7    -522.0    1044.0       136 
## 
## 
## Dispersion parameter for nbinom1 family (): 48.9 
## 
## Conditional model:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             3.11709    0.12941  24.086  < 2e-16 ***
## grass_forb_perc_scaled  0.43025    0.10936   3.934 8.35e-05 ***
## julian.date_scaled      0.08301    0.08634   0.961    0.336    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

# GLM #4: Number of inactive rodent burrows

First, plot poisson


``` r
inactiveburrows_lm_pois <- glmmTMB(num_inactive_burrows ~ grass_forb_perc_scaled + 
                    julian.date_scaled, 
                  family = poisson, #poisson
                  data = field_data)
```

Diagnostics


``` r
#simulate residuals

residuals_inactiveburrows_pois <- simulateResiduals(fittedModel = inactiveburrows_lm_pois, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-23-1.pdf)<!-- --> 

``` r
##residual plots show that there is overdispersion and  excessive outliers. Plot negative binomial
```

Plot negative binomial to see if that fixes the overdispersion.


``` r
inactiveburrows_lm_negbin <- glmmTMB(num_inactive_burrows ~ grass_forb_perc_scaled + 
                    julian.date_scaled, 
                  family = nbinom1, 
                  data = field_data)
```

Diagnostics


``` r
#simulate residuals

residuals_inactiveburrows_negbin <- simulateResiduals(fittedModel = inactiveburrows_lm_negbin, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-25-1.pdf)<!-- --> 

``` r
##residual plots show that there is no overdispersion or excessive outliers. Now check zero-inflation.
```

Test zero-inflation


``` r
testZeroInflation(residuals_inactiveburrows_negbin)
```

![](glmCH1_files/figure-latex/unnamed-chunk-26-1.pdf)<!-- --> 

```
## 
## 	DHARMa zero-inflation test via comparison to expected zeros with
## 	simulation under H0 = fitted model
## 
## data:  simulationOutput
## ratioObsSim = 0.94158, p-value = 0.664
## alternative hypothesis: two.sided
```

``` r
##p-value = 0.664. No strong evidence of zero-inflation. I'll try the zero-inflated model to see if it helps. 
```

Plot zero-inflated negative binomial and recheck diagnostics.


``` r
inactiveburrows_lm_zero_negbin <- glmmTMB(num_inactive_burrows ~ grass_forb_perc_scaled + 
                    julian.date_scaled, 
                    ziformula = ~1,  # zero-inflation model
                  family = nbinom1, 
                  data = field_data)
```

Compare AIC values between the 2 models


``` r
summary(inactiveburrows_lm_negbin)
```

```
##  Family: nbinom1  ( log )
## Formula:          
## num_inactive_burrows ~ grass_forb_perc_scaled + julian.date_scaled
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##     817.7     829.4    -404.8     809.7       136 
## 
## 
## Dispersion parameter for nbinom1 family ():   22 
## 
## Conditional model:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             2.20210    0.13894  15.850  < 2e-16 ***
## grass_forb_perc_scaled  0.36568    0.12113   3.019  0.00254 ** 
## julian.date_scaled      0.08254    0.09534   0.866  0.38663    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
summary(inactiveburrows_lm_zero_negbin)
```

```
##  Family: nbinom1  ( log )
## Formula:          
## num_inactive_burrows ~ grass_forb_perc_scaled + julian.date_scaled
## Zero inflation:                        ~1
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##     819.7     834.4    -404.8     809.7       135 
## 
## 
## Dispersion parameter for nbinom1 family ():   22 
## 
## Conditional model:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             2.20210    0.13894  15.849  < 2e-16 ***
## grass_forb_perc_scaled  0.36567    0.12113   3.019  0.00254 ** 
## julian.date_scaled      0.08253    0.09534   0.866  0.38667    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)   -18.04    5337.31  -0.003    0.997
```

``` r
##AIC and BIC values are slightly lover for negative binomial and there was no strong evidence for zero-inflation. Therefore, I think the negative binomal model is the best. 
```

Results for the final model


``` r
summary(inactiveburrows_lm_negbin)
```

```
##  Family: nbinom1  ( log )
## Formula:          
## num_inactive_burrows ~ grass_forb_perc_scaled + julian.date_scaled
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##     817.7     829.4    -404.8     809.7       136 
## 
## 
## Dispersion parameter for nbinom1 family ():   22 
## 
## Conditional model:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             2.20210    0.13894  15.850  < 2e-16 ***
## grass_forb_perc_scaled  0.36568    0.12113   3.019  0.00254 ** 
## julian.date_scaled      0.08254    0.09534   0.866  0.38663    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

# GLM #5: Floral abundance

Plot gamma regression since distribution of floral abundance across sites is positive and right-skewed.


``` r
floral_lm_tweedie <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled, 
                  family = tweedie(link = "log"), ##use tweedie family instead of Gamma since it can handle zeros and positive continuous data
                  data = field_data)
```

Diagnostics


``` r
#simulate residuals

residuals_floral_tweedie <- simulateResiduals(fittedModel = floral_lm_tweedie, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-31-1.pdf)<!-- --> 

``` r
##residual plots show that there is overdispersion and  excessive outliers.
```

Try plotting the log(floral abundance) as a linear regression


``` r
field_data$log_bombus_floral_abun <- log(field_data$bombus_floral_abun + 1e-4)

floral_lm_log <- lm(log_bombus_floral_abun ~ grass_forb_perc_scaled + julian.date_scaled, data = field_data)
```

Diagnostics


``` r
residuals_floral_log <- simulateResiduals(fittedModel = floral_lm_log, plot = TRUE)
```

![](glmCH1_files/figure-latex/unnamed-chunk-33-1.pdf)<!-- --> 

``` r
##fixed overdispersion and outliers. KS test suggests non-normality but I will check this in the Q-Q plot

library(car)
```

```
## Loading required package: carData
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```
## The following object is masked from 'package:purrr':
## 
##     some
```

``` r
qqPlot(floral_lm_log, main = "Q-Q Plot")
```

![](glmCH1_files/figure-latex/unnamed-chunk-33-2.pdf)<!-- --> 

```
## [1] 11 12
```

``` r
##Q-Q plot shows that residuals are still not normal. Need to try a different model.
```

Try running Tweedie model again but adjusting for overdispersion by adding a random effect for data structure


``` r
floral_glmm_tweedie <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|area/site_id/plot_id), 
                  family = tweedie(link = "log"), 
                  data = field_data)
```

Diagnostics


``` r
#simulate residuals

residuals_floral_tweedie_mixed <- simulateResiduals(fittedModel = floral_glmm_tweedie, plot = TRUE)
```

```
## Warning in newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,
## : Fitting terminated with step failure - check results carefully
```

![](glmCH1_files/figure-latex/unnamed-chunk-35-1.pdf)<!-- --> 

``` r
##residual plots show that there is still overdispersion. try different random effect.
```

Try simpler random effect structure (ignore site)


``` r
floral_glmm_tweedie2 <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|area/plot_id), 
                  family = tweedie(link = "log"), 
                  data = field_data)
```

Diagnostics –check for overdispersion


``` r
sim_res <- simulateResiduals(floral_glmm_tweedie2)
plot(sim_res)
```

![](glmCH1_files/figure-latex/unnamed-chunk-37-1.pdf)<!-- --> 

``` r
testDispersion(sim_res)
```

![](glmCH1_files/figure-latex/unnamed-chunk-37-2.pdf)<!-- --> 

```
## 
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
## 	simulated
## 
## data:  simulationOutput
## dispersion = 0.70199, p-value < 2.2e-16
## alternative hypothesis: two.sided
```

``` r
##still overdispersion. Keep original random effect structure for now but try modelling zero-inflation in model
```

Zero-inflated model


``` r
floral_glmm_tweediezero <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|area/site_id/plot_id), 
                    ziformula = ~1, #zero-inflated model
                  family = tweedie(link = "log"), 
                  data = field_data)
```

Diagnostics –check for overdispersion


``` r
sim_res <- simulateResiduals(floral_glmm_tweediezero)
plot(sim_res)
```

![](glmCH1_files/figure-latex/unnamed-chunk-39-1.pdf)<!-- --> 

``` r
testDispersion(sim_res)
```

![](glmCH1_files/figure-latex/unnamed-chunk-39-2.pdf)<!-- --> 

```
## 
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
## 	simulated
## 
## data:  simulationOutput
## dispersion = 0.95183, p-value = 0.584
## alternative hypothesis: two.sided
```

``` r
##no more overdispersion!
```

Check other random effect structures to determine which is best


``` r
#plot nested in area (drop site)

floral_glmm_tweediezero_plot <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|area/plot_id), 
                    ziformula = ~1, #zero-inflated model
                  family = tweedie(link = "log"), 
                  data = field_data)

#site nested in area (drop plot)

floral_glmm_tweediezero_site <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|area/site_id), 
                    ziformula = ~1, #zero-inflated model
                  family = tweedie(link = "log"), 
                  data = field_data)

#plot only

floral_glmm_tweediezero_plotonly <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|plot_id), 
                    ziformula = ~1, #zero-inflated model
                  family = tweedie(link = "log"), 
                  data = field_data)

#site only

floral_glmm_tweediezero_siteonly <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|site_id), 
                    ziformula = ~1, #zero-inflated model
                  family = tweedie(link = "log"), 
                  data = field_data)

#area only

floral_glmm_tweediezero_areaonly <- glmmTMB(bombus_floral_abun ~ grass_forb_perc_scaled + 
                    julian.date_scaled + (1|area), 
                    ziformula = ~1, #zero-inflated model
                  family = tweedie(link = "log"), 
                  data = field_data)

#check AIC
AIC(
  floral_glmm_tweediezero, 
  floral_glmm_tweediezero_plot,
  floral_glmm_tweediezero_site,
  floral_glmm_tweediezero_plotonly, 
  floral_glmm_tweediezero_siteonly, 
  floral_glmm_tweediezero_areaonly
)
```

```
##                                  df      AIC
## floral_glmm_tweediezero           9 516.2534
## floral_glmm_tweediezero_plot      8 514.2534
## floral_glmm_tweediezero_site      8 513.1628
## floral_glmm_tweediezero_plotonly  7 487.1094
## floral_glmm_tweediezero_siteonly  7 512.3448
## floral_glmm_tweediezero_areaonly  7 512.2534
```

``` r
##plot only model has lowest AIC--> go with that one!
```

Check overdispersion in plot only model


``` r
sim_res <- simulateResiduals(floral_glmm_tweediezero_plotonly)
plot(sim_res)
```

![](glmCH1_files/figure-latex/unnamed-chunk-41-1.pdf)<!-- --> 

``` r
testDispersion(sim_res)
```

![](glmCH1_files/figure-latex/unnamed-chunk-41-2.pdf)<!-- --> 

```
## 
## 	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
## 	simulated
## 
## data:  simulationOutput
## dispersion = 0.95435, p-value = 0.664
## alternative hypothesis: two.sided
```

``` r
##still no overdispersion
```

Look at summary for final model


``` r
summary(floral_glmm_tweediezero_plotonly)
```

```
##  Family: tweedie  ( log )
## Formula:          
## bombus_floral_abun ~ grass_forb_perc_scaled + julian.date_scaled +  
##     (1 | plot_id)
## Zero inflation:                      ~1
## Data: field_data
## 
##       AIC       BIC    logLik -2*log(L)  df.resid 
##     487.1     507.7    -236.6     473.1       133 
## 
## Random effects:
## 
## Conditional model:
##  Groups  Name        Variance  Std.Dev. 
##  plot_id (Intercept) 1.087e-10 1.043e-05
## Number of obs: 140, groups:  plot_id, 2
## 
## Dispersion parameter for tweedie family ():  0.5 
## 
## Conditional model:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             1.068626   0.040817  26.181   <2e-16 ***
## grass_forb_perc_scaled -0.009948   0.047577  -0.209    0.834    
## julian.date_scaled     -0.067595   0.041604  -1.625    0.104    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.6457     0.2318    -7.1 1.25e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
