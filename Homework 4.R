###############################################################
# Name: Danielle Senechal
# MAT-374: Data Analytics
# Homework 4
###############################################################
library(readr)
library(magrittr)
library(ggplot2)
library(Hmisc)
library(MASS)
library(car)

houses <- read_csv(
  "/Users/daniellesenechal/Documents/ECSU/Spring 2020/Data Analytics/Datasets/housing_train.csv")
# View(houses)

########################## Part 1 #############################

########## 1 ##########
# subset excluding central air
houses.sub <- subset(x = houses, select = c("SalePrice", "LotArea", "YearBuilt", "TotalBsmtSF", 
                                    "FullBath", "BedroomAbvGr", "Fireplaces", "GarageCars", 
                                    "PoolArea"))
# scatterplot matricies, excluding central air
pairs(~SalePrice + LotArea + YearBuilt + TotalBsmtSF + FullBath,
      data = houses.sub, pch = 19)
pairs(~SalePrice + BedroomAbvGr + Fireplaces + GarageCars + PoolArea,
      data = houses.sub, pch = 19)

########## 2 ##########
# correlation matrix
rcorr(as.matrix(houses.sub))

########## 3 ##########
plot(houses$YearBuilt, houses$SalePrice, xlab = "Year Built", ylab = "Sale Price", 
     main = "Sale Price Based off Year Built") # sale price vs year built plot
abline(lm(houses$SalePrice ~ houses$YearBuilt), col = "red") # regression line

reg1 <- lm(SalePrice ~ YearBuilt, data = houses) # SLR
summary(reg1) # 0.2734

########################## Part 2 #############################

########## 7 ##########
houses$CentralAir <- ifelse(test = houses$CentralAir == 'Y', yes = 1, no = 0) # convert to binary

########## 8 ##########
houses.sub2 <- subset(x = houses, select = c( "SalePrice", "LotArea", "YearBuilt", "TotalBsmtSF", 
                                              "CentralAir","FullBath", "BedroomAbvGr", "Fireplaces", 
                                              "GarageCars", "PoolArea"))

pairs(~SalePrice + LotArea + YearBuilt + TotalBsmtSF + CentralAir + FullBath,
      data = houses.sub2, pch = 19) # added binary CentralAir
pairs(~SalePrice + BedroomAbvGr + Fireplaces + GarageCars + PoolArea,
      data = houses.sub2, pch = 19) # same as above (1)

rcorr(as.matrix(houses.sub2))

reg2 <- lm(SalePrice ~ CentralAir, data = houses) # model for CentralAir predictor
summary(reg2)

########## 9 ##########
# stepwise
reg3 <- lm(SalePrice ~ LotArea, data = houses)
summary(reg3) # 0.06961
reg4 <- lm(SalePrice ~ TotalBsmtSF, data = houses)
summary(reg4) # 0.3765
reg5 <- lm(SalePrice ~ CentralAir, data = houses)
summary(reg5) # 0.06317
reg6 <- lm(SalePrice ~ FullBath, data = houses)
summary(reg6) # 0.3143
reg7 <- lm(SalePrice ~ BedroomAbvGr, data = houses)
summary(reg7) # 0.0283
reg8 <- lm(SalePrice ~ Fireplaces, data = houses)
summary(reg8) # 0.218
reg9 <- lm(SalePrice ~ GarageCars, data = houses)
summary(reg9) # 0.4101
reg10 <- lm(SalePrice ~ PoolArea, data = houses)
summary(reg10) # 0.008538
# highest R^2 is GarageCars, then TotalBsmtSF, then YearBuilt

########## 10 ##########
reg11 <- lm(SalePrice ~ GarageCars + TotalBsmtSF + YearBuilt,
           data = houses) # MR for three chosen variables

step <- stepAIC(reg11, direction="both"); step # stepwise to get formula

########## 11 ##########
summary(reg11) # adjusted R^2 and RSE

########## 12 ##########
vif(reg11) # VIF stats


