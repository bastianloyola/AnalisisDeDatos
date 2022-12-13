library(dplyr)
library(pROC)
library(caret)
library(tidyverse)
library(cluster)

bank <- read.csv('https://raw.githubusercontent.com/bastianloyola/AnalisisDeDatos/main/bank-additional/bank-additional/bank-additional-full.csv', sep=';')

#categorizar variables
bank$y <- factor(bank$y);
bank$job <- factor(bank$job);
bank$marital <- factor(bank$marital);
bank$education <- factor(bank$education);
bank$default <- factor(bank$default);
bank$housing <- factor(bank$housing);
bank$loan <- factor(bank$loan);
bank$contact <- factor(bank$contact);
bank$month <- factor(bank$month);
bank$day_of_week <- factor(bank$day_of_week);
bank$poutcome <- factor(bank$poutcome);

#Duplicados y removerlos
sum(duplicated(bank)) #12
#Existen duplicados

bank_unique <- unique(bank)
