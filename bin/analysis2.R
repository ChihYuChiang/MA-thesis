library(tidyverse)
library(data.table)

#Read in DT
DT <- fread("../data/survey2.csv")


#--Reverse (1-7 Likert) target responses
#Select target columns
targetColIndex <- matches("^Person.+((1_[24])|(2_[135]))$", vars=names(DT))

#Reverse 1-7 likert
reversed <- 8 - DT[, targetColIndex, with=FALSE]

#Assign back to DT
#Use parenthesis since the synax does not allow with=FALSE here
DT[, (targetColIndex) := reversed]


#--Combine sub-items
subColIndex_1 <- matches("^Person.+1_\\d$", vars=names(DT))
subColIndex_2 <- matches("^Person.+2_\\d$", vars=names(DT))
combined <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

