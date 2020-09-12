###############################################################
# Name: Danielle Senechal
# MAT-374: Data Analytics
# Project 1
###############################################################
library(readr)
library(ggplot2)
library(magrittr)
library(psych)
adult <- read.csv("Documents/ECSU/Spring 2020/Data Analytics/Datasets/adult01.csv") 
# View(adult)

########################## Task 1 #############################

########## 1 ##########
summary(adult) # missing var

describe(adult$age) # range, mean
median(adult$age) # median

# modes
summary(adult$workclass)
summary(adult$education)
summary(adult$marital.status)
summary(adult$income)

########## 2 ##########
age_z <- scale(adult$age); head(age_z)
age_z[(age_z >= 3),] # outliers above 3 (100)
age_z[(age_z <= -3),] # outliers below -3 (0)
abovethree <- adult[(age_z >= 3),] # rows corresponding to the outliers above 3
abovethree$age

########################## Task 2 #############################

########## 3 ##########
marr.inc <- table(adult$income, adult$marital.status); marr.inc
marr.inc.p <- prop.table(marr.inc, margin = 2) * 100; marr.inc.p
# make two way table, turn into proportion table, view

########## 4 ##########
# stacked
ggplot(adult) + geom_bar(aes(adult$marital.status, fill=adult$income)) +
  labs(x = "Marital Status", y = "Relative Frequency", fill = "Income Status", 
       title = "Marital Status Relative to Income Status")

# normalized
ggplot(adult) + geom_bar(aes(adult$marital.status, fill=adult$income),
                         position = "fill") +
  labs(x = "Marital Status", y = "Relative Frequency", fill = "Income Status", 
       title = "Marital Status Relative to Income Status")

########## 8 ##########
work.inc <- table(adult$income, adult$workclass); work.inc
work.inc.p <- prop.table(work.inc, margin = 2) * 100; work.inc.p
# make two way table, turn into proportion table, view

########## 9 ##########
# stacked
ggplot(adult) + geom_bar(aes(adult$workclass, fill=adult$income)) +
  labs(x = "Work Class", y = "Relative Frequency", fill = "Income Status", 
       title = "Work Class Relative to Income Status")

# normalized
ggplot(adult) + geom_bar(aes(adult$workclass, fill=adult$income),
                         position = "fill") +
  labs(x = "Work Class", y = "Relative Frequency", fill = "Income Status", 
       title = "Work Class Relative to Income Status")

########################## Task 3 #############################

########## 12 ##########
# stacked
ggplot(adult) + geom_histogram(aes(adult$age, fill=adult$income), 
                               binwidth = 1) +
  labs(x = "Age", y = "Relatice Frequency", fill = "Income Status", 
       title = "Income and Age, Senechal")

# normalized
ggplot(adult) + geom_histogram(aes(adult$age, fill=adult$income), 
                         position = "fill", binwidth = 1) +
  labs(x = "Age", y = "Relative Frequency", fill = "Income Status", 
       title = "Income and Age, Senechal")
