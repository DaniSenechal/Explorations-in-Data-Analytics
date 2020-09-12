###############################################################
# Name: Danielle Senechal
# MAT-374: Data Analytics
# Homework 2
###############################################################
library(readr)
library(ggplot2)
library(magrittr)
adult <- read_csv("Documents/ECSU/Spring 2020/Data Analytics/Datasets/adult.csv")
# View(adult)

############################## 1 ##############################
income.pt <- table(adult$income) %>% prop.table(); income.pt
  # take income column convert it to table, then proportion table

############################## 2 ##############################
married.pt <- table(adult$`marital-status`) %>% prop.table()
married.pt
  # take marriage column convert it to table, then proportion table

############################## 3 ##############################
income.df <- data.frame(income.pt); income.df
  # convert percentage table of incomes to dataframe, view

ggplot(income.df) + geom_col(aes(x=Var1, y=Freq, fill = Var1)) +
  ggtitle("Percentage of Incomes Above and Below 50K") +
  labs(x = "Income Status", y = "Frequency(%)") + 
  theme(legend.position = "none") 
  # Var1 and Freq came from the created dataframe

############################## 4 ############################## 
married.df <- data.frame(married.pt); married.df
  # convert percentage table of marital status to dataframe, view

ggplot(married.df) + geom_col(aes(x=Var1, y=Freq, fill = Var1)) +
  ggtitle("Frequency of the Marital Status Categories") +
  labs(x = "Marital Status", y = "Frequency(%)") + 
  theme(legend.position = "none") 
# Var1 and Freq came from the created dataframe

############################## 5 ############################## 
ggplot(adult) + geom_bar(aes(adult$`marital-status`, fill=adult$income)) +
  labs(x = "Marital Status", y = "Relative Frequency", fill = "Income Status", 
       title = "Marital Status Relative to Income Status")

############################## 6 ############################## 
# add position = "fill" to create normalized version
ggplot(adult) + geom_bar(aes(adult$`marital-status`, fill=adult$income),
                         position = "fill") +
  labs(x = "Marital Status", y = "Relative Frequency", fill = "Income Status", 
       title = "Marital Status Relative to Income Status")

  