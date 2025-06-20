library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(caret)
library(randomForest)
library(pROC)
library(rminer)
library(gridExtra)
library(corrplot)
library(reshape2)

data <- read.csv("D:/Shivani/Study/SEM-8/R LAB/creditcard/creditcard.csv", stringsAsFactors = F)

cc<-data
cc$Class<- as.factor(cc$Class)
head(cc)

data.frame(colSums(sapply(cc, is.na)))

summary(cc)

cc%>%
  filter(Amount == 0)%>%
  group_by(Class)%>%
  count()

cc %>%
  group_by(Class) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x= Class, y = freq, fill= Class)) + geom_bar(stat = "identity") + geom_text(aes(x = Class, y = freq, label = freq)) + ggtitle("Proportion of fraud transaction")

M <- cor(data)
corrplot(M, method = "circle")

all_time <- ggplot(cc, aes(x = Time)) + geom_histogram(aes(y =..density..)) + stat_density(geom = "line", aes(colour = "bla")) +
  ggtitle("Density of all transactoin over time")

fraud_time <- cc %>%
  filter(Class == 1) %>%
  ggplot(aes(x = Time)) + geom_histogram(aes(y =..density..)) + stat_density(geom = "line", aes(colour = "bla")) +
  ggtitle("Density of fraud transaction over time")

grid.arrange(all_time, fraud_time)

a<- ggplot(data = cc, aes(x = Amount)) + geom_histogram()
b<- ggplot(data = cc, aes(x = log(Amount))) + geom_histogram()
grid.arrange(a,b)

all_amount <- ggplot(cc, aes(x = log(Amount))) + geom_histogram(aes(y =..density..)) + stat_density(geom = "line", aes(colour = "bla")) + ggtitle("Density of all transaction over logged amount")

fraud_amount <- cc %>%
  filter(Class == 1) %>%
  ggplot(aes(x = log(Amount))) + geom_histogram(aes(y =..density..)) + stat_density(geom = "line", aes(colour = "bla")) + ggtitle("Density of fraud transaction over logged amount")

grid.arrange(all_amount, fraud_amount)

cc%>%
  filter(Amount <= 10, Class == 1)%>%
  ggplot(aes(x = Amount, fill = "red")) + geom_histogram() + ggtitle("Count of Fraud under $10 Amount")

