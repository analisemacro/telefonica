##########################
###### Exercícios SQL ####
##########################

rm(list=ls())

library(sqldf)
library(PASWR)
library(RSQLite)
library(DBI)

## Lendo um arquivo sqlite no R

con <- dbConnect(RSQLite::SQLite(), ":memory:")

dbListTables(con)

## Copiar um dataset na conexão

dbWriteTable(con, "mtcars", mtcars)

dbListTables(con)

dbListFields(con, "mtcars")

dbReadTable(con, "mtcars")

res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)

dbClearResult(res)

res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
while(!dbHasCompleted(res)){
  chunk <- dbFetch(res, n = 5)
  print(nrow(chunk))
}

dbClearResult(res)

dbDisconnect(con)

# Usando funções sql no R

data("titanic3")

# Count the number of rows in the 'titanic3' data 
# using sqldf function. Below is the R equivalent code to do the same.

sqldf("select count(*) from titanic3")

nrow(titanic3)

# Select all the columns and rows from 'titanic3' data 
# and put it into a variable 'TitanicData'. 

TitanicData <- sqldf("select * from titanic3")

TitanicData <- titanic3[ , ]

# Select the first two columns of the 'titanic3' data and 
# put it into a variable 'TitanicSubset2Cols'. 

colnames(titanic3)

TitanicSubset2Cols <- sqldf("select pclass,survived 
                            from titanic3")

# Print the first 6 rows of the 'titanic3' dataset using 
# sqldf function. 

sqldf("select * from titanic3 limit 6")

head(titanic3)

# Count the number of people in the 'titanic3' dataset 
# where the sex is female. 

sqldf("select count(*) from titanic3 where sex ='female' ")

nrow(titanic3[titanic3$sex=="female",])

# Count the number of people in the 'titanic3' dataset 
# where the sex is female and the port of embarkment is southampton.

sqldf("select count(*) from titanic3 where 
      (sex ='female' and embarked = 'Southampton') ")

nrow(titanic3[(titanic3$sex=="female" & 
                 titanic3$embarked=="Southampton"),])

# Calculate the total amount paid by female (where sex is female).

sqldf("select sum(fare) from titanic3 where sex='female'")

sum(titanic3$fare[titanic3$sex=="female"])

# Count the number of cabins in the ship.

sqldf("select count(distinct cabin) from titanic3 ")

length(unique(titanic3$cabin))

# Count the number of people in the ship whose name start with 'A'.

sqldf("select count(*) from titanic3 where name like 'A%' ")

nrow(titanic3[grep("^A", titanic3$name),])