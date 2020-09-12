###############################################################
# Name: Danielle Senechal
# MAT-374: Data Analytics
# Homework 3
###############################################################

library(readr)
library(caret)
library(magrittr)
set.seed(21)  # reproducable resutls

########################## Task 1 #############################
adult <- read_csv(
  "/Users/daniellesenechal/Documents/ECSU/Spring 2020/Data Analytics/Datasets/adult.csv")
# View(adult)

########## 1 ##########
train <- createDataPartition(y = adult$income, p = .8, list = F)  # 80%
income.train <- adult[train,]  # 80% to create training data
# View(income.train)
income.test <- adult[-train,]  # last 20% to create testing data

########## 2 ##########
t.income.train <- table(income.train$income); t.income.train
# training counts
t.income.test <- table(income.test$income); t.income.test
# testing counts
income.train.p <- table(income.train$income) %>% prop.table(); income.train.p * 100
# training proportions
income.test.p <- table(income.test$income) %>% prop.table(); income.test.p * 100
# testing proportions

########## 3 ##########
part.table <- matrix( c(15213, 3803, 4788, 1196), ncol=2)
# training and testing counts in table
colnames(part.table) <- c("<=50K", ">50K")
rownames(part.table) <- c("train", "test")
# name colummn and rows to reduce confusion
part.table

prop.test(part.table, correct = FALSE)  # find p-val to valiadate partition

########## 4 ##########
income.train$part <- rep("train", nrow(income.train))
income.test$part <- rep("test", nrow(income.test))
# append a specifier (train or test) to specify if row belongs to 
# training or testing data set

income.all <- rbind(income.train, income.test)
# last column states if row is in training or testing data
# View(income.all)

boxplot(age ~ as.factor(part), data = income.all)
# view boxplot to see distribution of age across the two groups

kruskal.test(age ~ as.factor(part), data = income.all)
# find p-val to valiadate partition

########################## Task 2 #############################
houses <- read_csv(
  "/Users/daniellesenechal/Documents/ECSU/Spring 2020/Data Analytics/Datasets/housing_train.csv")
# View(houses)

########## 1 ##########
t.CA <- table(houses$CentralAir); t.CA  # counts
p.CA <- prop.table(t.CA); p.CA * 100  # proportions

########## 2 ##########
(.2*1460-95)/.8  # find how many records need to be resampled

houses.rare.ind <-which(houses$CentralAir == "N")
# identify the rare records that will be resampled

houses.resampled.rare.ind <- sample(x = houses.rare.ind, size = 246.25, replace = TRUE)
# resample the rare records

houses.resample <- houses[houses.resampled.rare.ind,]
# extract resampled records

houses.balanced <- rbind(houses, houses.resample)  # merge datasets together

########## 3 ##########
t.CA.b <- table(houses.balanced$CentralAir)  # table of balanced data
p.CA.b <- prop.table(t.CA.b); p.CA.b * 100  # proportions of balanced data



