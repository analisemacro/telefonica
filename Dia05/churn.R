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

## Verificar o tenure
min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

