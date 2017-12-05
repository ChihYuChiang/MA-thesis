library(tidyverse)
library(data.table)

#Read in as DT
DT <- fread("../data/survey2.csv")




"
## Reverse (1-7 Likert) target responses
"
#--Select target columns
#Personality: 1_24 2_135; SDT: 1_246 2_246
targetColIndex <- matches("(^Person.+((1_[24])|(2_[135]))$)|(^SDT.+_[246]$)", vars=names(DT))
DT[, targetColIndex, with=FALSE]


#--Reverse 1-7 likert
reversed <- 8 - DT[, targetColIndex, with=FALSE]


#--Assign back to DT
#Use parenthesis since the synax does not allow with=FALSE here
DT[, (targetColIndex) := reversed]




"
## Combine sub-items
"
#--personalities (5 constructs; 2 items each)
#Computation
subColIndex_1 <- matches("^Person.+1_\\d$", vars=names(DT))
subColIndex_2 <- matches("^Person.+2_\\d$", vars=names(DT))
personalities <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

#Substitution
newColName <- gsub("1_", "", grep("^Person.+1_\\d$", names(DT), value=TRUE))
DT[, (newColName) := personalities]


#--SDT (3 constructs; 4 items each)
#Computation
subColIndex_1 <- matches("^SDT.+1_\\d$", vars=names(DT))
subColIndex_2 <- matches("^SDT.+2_\\d$", vars=names(DT))
temp <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

subColIndex_3 <- matches("^SDT.+1_[135]$", vars=names(temp))
subColIndex_4 <- matches("^SDT.+1_[246]$", vars=names(temp))
SDTs <- (temp[, subColIndex_3, with=FALSE] + temp[, subColIndex_4, with=FALSE]) / 2

#Substitution
newColName <- gsub("1_", "", grep("^SDT.+1_[123]$", names(DT), value=TRUE))
DT[, (newColName) := SDTs]

DT[, ]
t.test(df_player$game_agreeableness, df_player$real_agreeableness, paired=TRUE)