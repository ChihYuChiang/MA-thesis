library(tidyverse)
library(data.table)
library(colorspace)

#Read in as DT
DT <- fread("../data/survey2.csv")




"
## Reverse (1-7 Likert) target responses
"
#--Select target columns
#Personality: 1_24 2_135; SDT: 1_246 2_246
targetColIndex <- grep("(^Person.+((1_[24])|(2_[135]))$)|(^SDT.+_[246]$)", names(DT))


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
subColIndex_1 <- grep("^Person.+1_\\d$", names(DT))
subColIndex_2 <- grep("^Person.+2_\\d$", names(DT))
personalities <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

#Substitution
newColName <- gsub("1_", "", grep("^Person.+1_\\d$", names(DT), value=TRUE))
DT[, (newColName) := personalities]


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




"
## Compute gaps and sums
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




"
## Distribution
"
#--Personality
#Function for distribution
dist_personality <- function(DT, personality, types){
  #A map for personality code and str pairs
  personaCodec <- c("1"="Extraversion", "2"="Agreeableness", "3"="Conscientiousness;", "4"="Emotion stability", "5"="Openness")
  typeCodec <- c("InS"="In-game / Self", "OutS"="Real / Self", "IdS"="Ideal / Self", "InF"="In-game / Fellow", "OutF"="Real / Fellow", "SteS"="Stereotype / Public")
  
  #Acquire specific columns of that personality
  targetColIndex <- matches(sprintf("^Person.+-%s$", personality), vars=names(DT))
  
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("Person%s-%s", type, personality)), fill=toString(which(types == type))),
                   binwidth=0.5, alpha=0.6)
  }
  geom_hists <- lapply(types, make_hist)
  
  #Use a list to add ggplot components
  ggplot(data=DT[, targetColIndex, with=FALSE]) +
    geom_hists +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7), limits=c(0.5, 7.5)) +
    labs(x="score", title=personaCodec[toString(personality)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(typeCodec[unlist(types)])) +
    theme_minimal()
}

#Function call
#InS OutS IdS InF OutF SteS
dist_personality(DT, 4, list("IdS"))


#--SDT
#Function for distribution
dist_SDT <- function(DT, SDT, types){
  #A map for personality code and str pairs
  SDTCodec <- c("1"="Autonomy", "2"="Relatedness", "3"="Competence")
  typeCodec <- c("In"="In-game", "Out"="Real", "Id"="Ideal")
  
  #Acquire specific columns of that personality
  targetColIndex <- matches(sprintf("^SDT.+-%s$", SDT), vars=names(DT))
  
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("SDT%s-%s", type, SDT)), fill=toString(which(types == type))),
                   binwidth=0.5, alpha=0.6)
  }
  geom_hists <- lapply(types, make_hist)
  
  #Use a list to add ggplot components
  ggplot(data=DT[, targetColIndex, with=FALSE]) +
    geom_hists +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7), limits=c(0.5, 7.5)) +
    labs(x="score", title=SDTCodec[toString(SDT)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(typeCodec[unlist(types)])) +
    theme_minimal()
}

#Function call
#In Out Id
dist_SDT(DT, 3, list("In", "Out"))


#--Filter by GProfile Demo Relation and Pref