#################################################
###### Análise de Retenção de Clientes ##########

## Pacotes utilizados

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(MASS)
library(caret)
library(randomForest)
library(party)

## Importar dados

churn <- read.csv('Telco-Customer-Churn.csv')

######## Tratamento dos dados ######

str(churn)

sapply(churn, function(x) sum(is.na(x)))
churn = churn[complete.cases(churn),]

## Retirar "No internet service"

cols_recode1 = c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

