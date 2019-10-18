Stata and R codes

Stata code for modelling the quantity of ships’ demolition
***1. Time and variables generation***

***Importing the data from Excel***

import excel "C:\Users\nesty\Desktop\data.xls", sheet("data") firstrow

***Generating and setting the Time variable from Jan 2000 to Aug 2018***

gen time = tm(2000m1)+t
tsset time, monthly
***Generating a natural logarithm of BDI***
gen LnBDI = log(BDI)
***Generating a dummy variable DSlope: (0) - when DemoN decreases and Oil increases; (1) - when DemoN increases and Oil increases***
gen DSlope = 1
replace DSlope = 0 if time <= tm(2005m3)
replace DSlope = 0 if time >= tm(2008m10) & time<= tm(2010m9)
replace DSlope = 0 if time >= tm(2012m7)

***Generating a dummy variable D2004: (1) - before Feb 2004; (0) - after Feb 2004***

gen D2004 = 0
replace D2004 = 1 if time <= tm(2004m2)
***Generating a dummy variable D2008: (0) - before Dec 2008; (1) - after Dec 2008***
gen D2008 = 0
replace D2008 = 1 if time >= tm(2008m12)
***Generating the dummy variables related to the Oil and Season***
gen DSlope_Oil = DSlope * Oil
gen D2004_Season = D2004 * Season
gen D2008_Season = D2008 * Season

***2. The relationships among variables***

***Dynamics of numbers of bulker carriers (Bulk) and containerships (Cont) demolitions, and their joint trend (DemoN) from Jan 2000 to Aug 2018***

tsline Bulk || tsline Cont || tsline DemoN

***Dynamics of number of demolitions’ joint trend (DemoN) and crude oil monthly price (Oil) from January 2000 to August 2018***
tsline DemoN || tsline Oil

***Dynamics of Baltic Dry Index (BDI) from January 2000 to August 2018***

tsline BDI

***Dynamics of World Trade (WTrade) from January 2000 to August 2018***

tsline WTrade

***Scatter diagram between DemoN and Oil with linear and quadratic trends***

scatter DemoN Oil || lfit DemoN Oil || qfit DemoN Oil

***Scatter diagram between DemoN and BDI with linear and quadratic trends***

scatter DemoN BDI || lfit DemoN BDI || qfit DemoN BDI

***Histogram of BDI with Normal distribution theoretical curve***

hist BDI, normal

***Histogram of LnBDI with Normal distribution theoretical curve***

hist LnBDI, normal

***Scatter diagram between DemoN and LnBDI with linear and quadratic trends***

scatter DemoN LnBDI || lfit DemoN LnBDI || qfit DemoN LnBDI

***Scatter diagram between DemoN and WTrade with linear and quadratic trends***

scatter DemoN WTrade || lfit DemoN WTrade || qfit DemoN WTrade

***Correlation matrix between variables***

pwcorr DemoN Oil LnBDI WTrade Season D2004 D2008 DSlope, sig

***3. DemoN's time series analysis***
***ACF of DemoN***
ac DemoN

***PACF of DemoN***

pac DemoN

***Periodogram of DemoN***

pergram DemoN

***ADF tests for stationarity of DemoN***

dfuller DemoN, noconstant
dfuller DemoN, constant
dfuller DemoN, regress
dfuller DemoN, trend regress

***HEGY-tests for seasonality of DemoN***

hegy DemoN, level(5)
hegy DemoN, gls det(const)

***HEGY-test for seasonality of DemoN with seasonal intercepts included***

hegy DemoN, gls det(seas)

***HEGY-test for seasonality of DemoN with seasonal intercepts and zero frequency trend***

hegy DemoN, gls det(strend)

***HEGY-test for seasonality of DemoN with seasonal intercepts and trends included***

hegy DemoN, gls det(mult)

***4. Structural breaks***

***Test for a structural break with an unknown break date in the model with DemoN as a depvar***

reg DemoN Oil LnBDI WTrade Season
estat sbsingle

***DemoN as a depvar: Chow breakpoint test for a known moment of time: Feb 2004***

chowreg DemoN Oil LnBDI WTrade Season, d(50)

***DemoN as a depvar: Chow breakpoint test for a known moment of time: Dec 2008***

chowreg DemoN Oil LnBDI WTrade Season, d(108)

***Bulk as a depvar: Chow breakpoint test for a known moment of time: Feb 2004***

chowreg Bulk Oil LnBDI WTrade Season, d(50)

***Bulk as a depvar: Chow breakpoint test for a known moment of time: Dec 2008***

chowreg Bulk Oil LnBDI WTrade Season, d(108)

***Cont as a depvar: Chow breakpoint test for a known moment of time: 
Feb 2004***

chowreg Cont Oil LnBDI WTrade Season, d(50)
***Cont as a depvar: Chow breakpoint test for a known moment of time: Dec 2008***

chowreg Cont Oil LnBDI WTrade Season, d(108)

***5. OLS regression***

***OLS regression model with all variables included***
reg DemoN Oil LnBDI WTrade Season D2004 D2008 DSlope D2004_Season D2008_Season DSlope_Oil
estat vif

***Wald tests for significance of dummies***

test D2004 == 0
test D2008 == 0
test DSlope == 0
test D2004_Season == 0
test D2008_Season == 0
test DSlope_Oil == 0

***OLS regression models for 6 subsets and 2 joint models***

reg DemoN Oil LnBDI WTrade Season if time <= tm(2004m1)
estimates store m1, title(Model 1)
reg DemoN Oil LnBDI WTrade Season if time >= tm(2004m2) & time <= tm(2005m3)
estimates store m2, title(Model 2)
reg DemoN Oil LnBDI WTrade Season if time >= tm(2005m4) & time <= tm(2008m11)
estimates store m3, title(Model 3)
reg DemoN Oil LnBDI WTrade Season if time >= tm(2008m12) & time <= tm(2010m9)
estimates store m4, title(Model 4)
reg DemoN Oil LnBDI WTrade Season if time >= tm(2010m10) & time <= tm(2012m6)
estimates store m5, title(Model 5)
reg DemoN Oil LnBDI WTrade Season if time >= tm(2012m7)
estimates store m6, title(Model 6)
reg DemoN Oil LnBDI WTrade Season D2004 D2008
estimates store m7, title(Joint OLS 1)
reg DemoN Oil LnBDI WTrade Season D2004 D2008 DSlope DSlope_Oil
estimates store m8, title(Joint OLS 2)
estout m1 m2 m3 m4 m5 m6 m7 m8, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons constant) stats(r2 df_r aic bic rmse, fmt(3 0 1 1 3) label(R-sqr dfres AIC BIC RMSE))

***Final OLS regression model: Jarque-Bera’s test for normality of residuals, RESET-test for nonlinearity***

reg DemoN Oil LnBDI WTrade Season D2004 D2008
estimates store OLS, title(OLS)
predict Ohat
rmse DemoN Ohat
predict Ores, residuals
jb Ores
reset DemoN Oil LnBDI WTrade Season D2004 D2008
fitstat




***6. Poisson regression***

***Poisson models for 6 subsets and 2 joint models***

poisson DemoN Oil LnBDI WTrade Season if time <= tm(2004m1)
estimates store p1, title(Poisson 1)
poisson DemoN Oil LnBDI WTrade Season if time >= tm(2004m2) & time <= tm(2005m3)
estimates store p2, title(Poisson 2)
poisson DemoN Oil LnBDI WTrade Season if time >= tm(2005m4) & time <= tm(2008m11)
estimates store p3, title(Poisson 3)
poisson DemoN Oil LnBDI WTrade Season if time >= tm(2008m12) & time <= tm(2010m9)
estimates store p4, title(Poisson 4)
poisson DemoN Oil LnBDI WTrade Season if time >= tm(2010m10) & time <= tm(2012m6)
estimates store p5, title(Poisson 5)
poisson DemoN Oil LnBDI WTrade Season if time >= tm(2012m7)
estimates store p6, title(Poisson 6)
poisson DemoN Oil LnBDI WTrade Season D2004 D2008
estimates store p7, title(Joint Poisson 1)
poisson DemoN Oil LnBDI WTrade Season D2004 D2008 DSlope DSlope_Oil
estimates store p8, title(Joint Poisson 2)
estout p1 p2 p3 p4 p5 p6 p7 p8, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons constant) stats(r2 df_r aic bic rmse, fmt(3 0 1 1 3) label(R-sqr dfres AIC BIC RMSE))

***Final Poisson regression model***

poisson DemoN Oil LnBDI WTrade Season D2008 
estimates store Poisson, title(Poisson)
poisson, irr
fitstat
predict Phat
rmse DemoN Phat

***7. Negative binomial regression***

***Negative binomial regression model***

nbreg DemoN Oil LnBDI WTrade Season D2008 
estimates store NBreg, title(NBreg)
fitstat
predict NBhat
rmse DemoN NBhat
nbreg, irr

***Joint table of comparison between OLS, Poisson and Negative binomial regressions***

estout OLS Poisson NBreg, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons constant) stats(r2 df_r aic bic rmse, fmt(3 0 1 1 3) label(R-sqr dfres AIC BIC RMSE))



***8. Train and test sets’ splitting: the results of prediction***

poisson DemoN Oil LnBDI WTrade Season D2008 
estimates store Poisson, title(Poisson)
***Train test ends on 156th observation: 156 for train and 68 for test sets (70 % vs. 30 % approximately)***
gen train = 1
replace train = 0 if time>=tm(2013m1)

reg DemoN Oil LnBDI WTrade Season D2008 if train
predict OLStesthat if !train
sum OLStesthat
rmse DemoN OLStesthat

poisson DemoN Oil LnBDI WTrade Season D2008 if train
predict Ptesthat if !train
sum Ptesthat
rmse DemoN Ptesthat

nbreg DemoN Oil LnBDI WTrade Season D2008 if train
predict NBtesthat if !train
sum NBtesthat
rmse DemoN NBtesthat

Stata code for Logistic regression

#Importing data from Excel

import excel "C:\Users\nesty\Desktop\pdata.xls", sheet("pdata") firstrow

#Making Dwt to be a real number

gen DWT = real(Dwt)

#Building a logit-model

logit Demolished DWT Age i.Type

#Testing the overall significance of Type

test 2.Type 3.Type

logit, or

# Calculating the margins

margins Type, atmeans

fitstat
