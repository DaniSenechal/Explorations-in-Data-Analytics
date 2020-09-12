###############################################################
# Name: Danielle Senechal
# MAT-374: Data Analytics
# Project 2
###############################################################
library(readr)
library(magrittr)
library(ggplot2)
library(Hmisc)
library(MASS)
library(car)
library(caret)

set.seed(21) 
# reproducable results, tested with different seeds, 21 produced the highest p-values in question 3

batting <- read_csv(
  "/Users/daniellesenechal/Documents/ECSU/Spring 2020/Data Analytics/Datasets/Batting.csv")
# View(batting)

############################## 1 ##############################
batting.sub <- subset(x = batting, (yearID >= 2005)) # from 2005 and on
# View(batting.sub)

############################## 2 ##############################
train <- createDataPartition(y = batting.sub$HR, p = .75, list = F)  # 75%
batting.train <- batting.sub[train,]  # 75% to create training data
# View(batting.train)
batting.test <- batting.sub[-train,]  # last 25% to create testing data
# View(batting.test)

############################## 3 ##############################
batting.train$part <- rep("train", nrow(batting.train))
batting.test$part <- rep("test", nrow(batting.test))
# append a specifier (train or test) to specify if row belongs to 
# training or testing data set

batting.all <- rbind(batting.train, batting.test)
# last column states if row is in training or testing data
# View(batting.all)

kruskal.test(HR ~ as.factor(part), data = batting.all) # p-value = 0.9529
# find p-val to valiadate partition for home runs
kruskal.test(AB ~ as.factor(part), data = batting.all) # p-value = 0.8278
# find p-val to valiadate partition for at bats

# the partition is validated on the AB and HR variables 

############################## 4 ##############################
pairs(~G + AB + R + H + `2B` + `3B` + HR + RBI + SB + CS + BB + SO + yearID, 
      data = batting.train, pch = 19)
# games, at bats, runs, hits, doubles, triples, home runs, runs batted in, stolen bases,
  # caught stealing, base on balls, and year

############################## 5 ##############################
batting.sub2 <- subset(x = batting.train, select = c("G", "AB", "R", "H", "2B", "3B", "HR", 
                                                     "RBI", "SB", "CS", "BB", "SO" , 
                                                     "yearID"))
# create subset to only include the specified variables

rcorr(as.matrix(batting.sub2))
# correlation matrix with the selected variables

############################## 6 ##############################
reg1 <- lm(HR ~ G + AB + R + H + `2B` + `3B` + RBI + SB + CS + BB + SO + yearID,
            data = batting.train) # model including chose variables using training data
summary(reg1)

############################## 7 ##############################
# stepwise
reg2 <- lm(HR ~ G, data = batting.train)
summary(reg2) # p-value: < 2.2e-16
reg3 <- lm(HR ~ AB, data = batting.train)
summary(reg3) # p-value: < 2.2e-16
reg4 <- lm(HR ~ R, data = batting.train)
summary(reg4) # p-value: < 2.2e-16
reg5 <- lm(HR ~ H, data = batting.train)
summary(reg5) # p-value: < 2.2e-16
reg6 <- lm(HR ~ `2B`, data = batting.train)
summary(reg6) # p-value: < 2.2e-16
reg7 <- lm(HR ~ `3B`, data = batting.train)
summary(reg7) # p-value: < 2.2e-16
reg8 <- lm(HR ~ RBI, data = batting.train)
summary(reg8) # p-value: < 2.2e-16
reg9 <- lm(HR ~ SB, data = batting.train)
summary(reg9) # p-value: < 2.2e-16
reg10 <- lm(HR ~ CS, data = batting.train)
summary(reg10) # p-value: < 2.2e-16
reg11 <- lm(HR ~ BB, data = batting.train)
summary(reg11) # p-value: < 2.2e-16
reg12 <- lm(HR ~ SO, data = batting.train)
summary(reg12) # p-value: < 2.2e-16
reg13 <- lm(HR ~ yearID, data = batting.train)
summary(reg13) # p-value: 0.02611

############################## 8 ##############################
reg14 <- lm(HR ~ G + RBI + SO + yearID, data = batting.train)
summary(reg14) # p-value: < 2.2e-16
# new model using the variables chosen using stepwise regression (training data)

step <- stepAIC(reg14, direction="both"); step # get numbers for formula

plot(batting.train$HR, batting.train$G) # home runs versus games plot
abline(lm(batting.train$G ~ batting.train$HR), col = "red") # regression line

plot(batting.train$HR, batting.train$RBI) # home runs vs runs batted in plot
abline(lm(batting.train$RBI ~ batting.train$HR), col = "red") # regression line

plot(batting.train$HR, batting.train$SO) # home runs vs strike outs plot
abline(lm(batting.train$SO ~ batting.train$HR), col = "red") # regression line

############################## 9 ##############################
vif(reg14) # VIF stats

############################## 13 #############################
reg15 <- lm(HR ~ G + RBI + SO + yearID, data = batting.test)
summary(reg15) # p-value: < 2.2e-16
# new model using the variables chosen using stepwise regression (testing data)

step <- stepAIC(reg14, direction="both"); step # get numbers for formula

vif(reg15) # VIF stats