###########################
##### Exemplo de BI #######

# Análise a venda de telefones celulares em uma Loja de um player 
# do mercado brasileiro

# Limpeza do Environment
rm(list = ls(all.names = TRUE))

# Pacotes
library(lattice)
library(ggplot2)
library(sqldf)
library(plotly)


# Importação dos Dados
data <- read.delim(file = 'purchases.txt', 
                   header = FALSE, sep = '\t', dec = '.')

# Visualização dos dados
str(data)
summary(data)
head(data)
tail(data)

# Tratamento inicial 

colnames(data) <- c('customer_id', 
                    'purchase_amount', 
                    'date_of_purchase')

data$date_of_purchase <- as.Date(data$date_of_purchase, 
                                 "%Y-%m-%d")


data$days_since <- as.numeric(difftime(time1 = "2016-01-01",
                                       time2 = data$date_of_purchase,
                                       units = "days"))

str(data)
summary(data)

# Visualização de Dados

xyplot(purchase_amount ~ date_of_purchase, data)

ggplot(data, aes(date_of_purchase, purchase_amount))+
  geom_point()


# Criando indicadores de marketing

# Compute recency, frequency, and average purchase amount
customers <- sqldf("SELECT customer_id,
                   MIN(days_since) AS 'recency',
                   COUNT(*) AS 'frequency',
                   AVG(purchase_amount) AS 'amount'
                   FROM data GROUP BY 1")

tail(customers)
summary(customers)
summary(data)

hist(customers$recency)
hist(customers$amount)
hist(customers$frequency)

xyplot(frequency ~ amount, customers)
xyplot(recency ~ amount, customers)

ggplot(customers, aes(amount, recency))+
  geom_point()


ggplotly()

densityplot(~ amount, customers)
densityplot(~ frequency, customers)

hist(customers$amount, freq=F)
lines(density(customers$amount), col='blue')


new_data <- customers

head(new_data)

row.names(new_data) <- new_data$customer_id

tail(new_data)

new_data$customer_id <- NULL

head(new_data)

new_data$amount <- log(new_data$amount)
new_data$frequency <- log(new_data$frequency)

new_data <- scale(new_data)

sample <- seq(1, nrow(new_data), by = 10)

customers_sample <- customers[sample, ]
new_data_sample  <- new_data[sample, ]

d <- dist(new_data_sample)

c <- hclust(d, method="ward.D2")

plot(c)

members <- cutree(c, k = 3)

table(members)

aggregate(customers_sample[, 2:4], 
          by = list(members), mean)
