library(tidyverse)
library(data.table)

#Read in as DT
#Skip 2 for codec
DT <- fread("../../data/survey2.csv", skip=2)[
  MTurkCode != "", ,] #Filter

#Read in codec
codec <- as.data.table(t(fread("../../data/survey2.csv", nrows=1)), keep.rownames=TRUE)
colnames(codec) <- c("Variable", "Description")







"
----------------------------------------------------------------------
## Initialization
----------------------------------------------------------------------
"
"
### Reverse (1-7 Likert) target responses
"
#--Select target columns
#Personality: 1_24 2_135; SDT: 1_246 2_246; Preference: -2
targetColIndex <- grep("(^Person.+((1_[24])|(2_[135]))$)|(^SDT.+_[246]$)|(^Pref.-2)", names(DT), value=TRUE)


#--Reverse 1-7 likert
reversed <- 8 - DT[, targetColIndex, with=FALSE]


#--Assign back to DT
#Use parenthesis since the synax does not allow with=FALSE here
DT[, (targetColIndex) := reversed]




"
### Combine sub-items
"
#--personalities (5 constructs; 2 items each)
#Computation
subColIndex_1 <- grep("^Person.+1_\\d$", names(DT))
subColIndex_2 <- grep("^Person.+2_\\d$", names(DT))
personalities <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

#Substitution
newColName <- gsub("1_", "", grep("^Person.+1_\\d$", names(DT), value=TRUE))
DT[, (newColName) := personalities]

#Update codec
codec <- rbind(codec, list("PersonXY-Z",
                           "Combined personality measurement.
                            X = {In: in-game, Out: real, Id: ideal, Ste: stereotype}
                            Y = {S: self-version, F: fellow-version}
                            Z = {1: extraversion, 2: agreeableness, 3: conscientiousness, 4: emotion stability, 5: openness, sum: summation}"))


#--SDT (3 constructs; 4 items each)
#Computation
subColIndex_1 <- grep("^SDT.+1_[135]$", names(DT))
subColIndex_2 <- grep("^SDT.+1_[246]$", names(DT))
subColIndex_3 <- grep("^SDT.+2_[135]$", names(DT))
subColIndex_4 <- grep("^SDT.+2_[246]$", names(DT))
SDTs <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE] + DT[, subColIndex_3, with=FALSE] + DT[, subColIndex_4, with=FALSE]) / 4

#Substitution
newColName <- gsub("1_", "", grep("^SDT.+1_[123]$", names(DT), value=TRUE))
DT[, (newColName) := SDTs]

#Update codec
codec <- rbind(codec, list("SDTX-Z",
                           "Combined SDT measurement.
                            X = {In: in-game, Out: real, Id: ideal}
                            Z = {1: autonomy, 2: relatedness, 3: competence, sum: summation}"))


#--Preference (5 items)
DT[, "PrefS-a1" := rowMeans(.SD), .SDcols=grep("^PrefS-\\d$", names(DT))] #All 5 measures
DT[, "PrefS-a2" := rowMeans(.SD), .SDcols=grep("^PrefS-[1234]$", names(DT))] #Except play frequency
DT[, "PrefF-a1" := rowMeans(.SD), .SDcols=grep("^PrefF-\\d$", names(DT))]
DT[, "PrefF-a2" := rowMeans(.SD), .SDcols=grep("^PrefF-[1234]$", names(DT))]

#Update codec
codec <- rbind(codec, list("PrefX-aZ",
                           "Combined preference measurement.
                            X = {S: self-version, F: fellow-version}
                            Z = {1: all 5 measures, 2: exclude play frequency}"))




"
### Compute gaps and sums
"
#--Personality
colIndex_InS <- grep("^PersonInS-\\d$", names(DT))
colIndex_OutS <- grep("^PersonOutS-\\d$", names(DT))
colIndex_IdS <- grep("^PersonIdS-\\d$", names(DT))

#InS - OutS
InSOutS <- DT[, colIndex_InS, with=FALSE] - DT[, colIndex_OutS, with=FALSE]
newColName <- gsub("InS", "InSOutS", grep("^PersonInS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := InSOutS]

#IdS - InS
IdSInS <- DT[, colIndex_IdS, with=FALSE] - DT[, colIndex_InS, with=FALSE]
newColName <- gsub("InS", "IdSInS", grep("^PersonInS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdSInS]

#IdS - OutS
IdSOutS <- DT[, colIndex_IdS, with=FALSE] - DT[, colIndex_OutS, with=FALSE]
newColName <- gsub("InS", "IdSOutS", grep("^PersonInS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdSOutS]

#Gap sum
DT[, "PersonInSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonInSOutS-\\d$", names(DT))]
DT[, "PersonIdSInS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSInS-\\d$", names(DT))]
DT[, "PersonIdSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSOutS-\\d$", names(DT))]

#Gap absolute sum
DT[, "PersonInSOutS-absum" := rowSums(abs(.SD)), .SDcols=grep("^PersonInSOutS-\\d$", names(DT))]
DT[, "PersonIdSInS-absum" := rowSums(abs(.SD)), .SDcols=grep("^PersonIdSInS-\\d$", names(DT))]
DT[, "PersonIdSOutS-absum" := rowSums(abs(.SD)), .SDcols=grep("^PersonIdSOutS-\\d$", names(DT))]

#InS, OutS, IdS sum
DT[, "PersonInS-sum" := rowSums(.SD), .SDcols=grep("^PersonInS-\\d$", names(DT))]
DT[, "PersonOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonOutS-\\d$", names(DT))]
DT[, "PersonIdS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdS-\\d$", names(DT))]

#Update codec
codec <- rbind(codec, list("PersonOO-Z",
                           "Personality gaps.
                            OO = {InSOutS, IdSInS, IdSOutS; eg. IdSInS: ideal(self-version) - in-game(self-version)}
                            Z = {1: extraversion, 2: agreeableness, 3: conscientiousness, 4: emotion stability, 5: openness, sum: summation, absum: absolute then summation}"))


#--SDT
colIndex_In <- grep("^SDTIn-\\d$", names(DT))
colIndex_Out <- grep("^SDTOut-\\d$", names(DT))
colIndex_Id <- grep("^SDTId-\\d$", names(DT))

#In - Out
InOut <- DT[, colIndex_In, with=FALSE] - DT[, colIndex_Out, with=FALSE]
newColName <- gsub("In", "InOut", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := InOut]

#Id - In
IdIn <- DT[, colIndex_Id, with=FALSE] - DT[, colIndex_In, with=FALSE]
newColName <- gsub("In", "IdIn", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdIn]

#Id - Out
IdOut <- DT[, colIndex_Id, with=FALSE] - DT[, colIndex_Out, with=FALSE]
newColName <- gsub("In", "IdOut", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdOut]

#Gap sum
DT[, "SDTInOut-sum" := rowSums(.SD), .SDcols=grep("^SDTInOut-\\d$", names(DT))]
DT[, "SDTIdIn-sum" := rowSums(.SD), .SDcols=grep("^SDTIdIn-\\d$", names(DT))]
DT[, "SDTIdOut-sum" := rowSums(.SD), .SDcols=grep("^SDTIdOut-\\d$", names(DT))]

#Gap absolute sum
DT[, "SDTInOut-absum" := rowSums(abs(.SD)), .SDcols=grep("^SDTInOut-\\d$", names(DT))]
DT[, "SDTIdIn-absum" := rowSums(abs(.SD)), .SDcols=grep("^SDTIdIn-\\d$", names(DT))]
DT[, "SDTIdOut-absum" := rowSums(abs(.SD)), .SDcols=grep("^SDTIdOut-\\d$", names(DT))]

#In, Out, Id sum
DT[, "SDTIn-sum" := rowSums(.SD), .SDcols=grep("^SDTIn-\\d$", names(DT))]
DT[, "SDTOut-sum" := rowSums(.SD), .SDcols=grep("^SDTOut-\\d$", names(DT))]
DT[, "SDTId-sum" := rowSums(.SD), .SDcols=grep("^SDTId-\\d$", names(DT))]

#Update codec
codec <- rbind(codec, list("SDTOO-Z",
                           "SDT gaps.
                            OO = {InOut, IdIn, IdOut; eg. IdIn: ideal - in-game}
                            Z = {1: autonomy, 2: relatedness, 3: competence, sum: summation, absum: absolute then summation}"))


"
### Clean temp vars
"
rm(list=ls()[which(ls() != "DT" & ls() != "codec")]) #Preserve only DT and codec
