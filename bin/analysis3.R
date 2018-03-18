library(tidyverse)
library(data.table)

#Read in as DT
#Skip 2 for codec
DT <- fread("../data/raw_survey3/survey3.csv", skip=2)

#Read in codec
codec <- as.data.table(t(fread("../raw_survey3/survey3.csv", nrows=1)), keep.rownames=TRUE)
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
#Personality: 1_24 2_135
targetColIndex <- grep("^Person[A-Za-z]{2,4}-((1_[24])|(2_[135]))$", names(DT), value=TRUE)


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
subColIndex_1 <- grep("^Person[A-Za-z]{2,4}-1_\\d$", names(DT))
subColIndex_2 <- grep("^Person[A-Za-z]{2,4}-2_\\d$", names(DT))
personalities <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

#Substitution
newColName <- gsub("1_", "", grep("^Person[A-Za-z]{2,4}-1_\\d$", names(DT), value=TRUE))
DT[, (newColName) := personalities]




"
### Compute gaps and sums
"
#--Personality
colIndex_Hb <- grep("^PersonHb-\\d$", names(DT))
colIndex_Lch <- grep("^PersonLch-\\d$", names(DT))
colIndex_OutS <- grep("^PersonOutS-\\d$", names(DT))
colIndex_IdS <- grep("^PersonIdS-\\d$", names(DT))


#Hb - OutS (original and absolute)
HbOutS <- DT[, colIndex_Hb, with=FALSE] - DT[, colIndex_OutS, with=FALSE]
newColName <- gsub("IdS", "HbOutS", grep("^PersonIdS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := HbOutS]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(HbOutS)]

#Lch - OutS (original and absolute)
LchOutS <- DT[, colIndex_Lch, with=FALSE] - DT[, colIndex_OutS, with=FALSE]
newColName <- gsub("IdS", "LchOutS", grep("^PersonIdS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := LchOutS]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(LchOutS)]

#IdS - OutS (original and absolute)
IdSOutS <- DT[, colIndex_IdS, with=FALSE] - DT[, colIndex_OutS, with=FALSE]
newColName <- gsub("IdS", "IdSOutS", grep("^PersonIdS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdSOutS]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(IdSOutS)]

#IdS - Hb (original and absolute)
IdSHb <- DT[, colIndex_IdS, with=FALSE] - DT[, colIndex_Hb, with=FALSE]
newColName <- gsub("IdS", "IdSHb", grep("^PersonIdS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdSHb]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(IdSHb)]

#Gap sum
DT[, "PersonHbOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonHbOutS-\\d$", names(DT))]
DT[, "PersonLchSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonLchOutS-\\d$", names(DT))]
DT[, "PersonIdSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSOutS-\\d$", names(DT))]
DT[, "PersonIdSHb-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSHb-\\d$", names(DT))]

#Gap absolute sum
DT[, "PersonHbOutS-absum" := rowSums(.SD), .SDcols=grep("^PersonHbOutS-ab\\d$", names(DT))]
DT[, "PersonLchSOutS-absum" := rowSums(.SD), .SDcols=grep("^PersonLchOutS-ab\\d$", names(DT))]
DT[, "PersonIdSOutS-absum" := rowSums(.SD), .SDcols=grep("^PersonIdSOutS-ab\\d$", names(DT))]
DT[, "PersonIdSHb-absum" := rowSums(.SD), .SDcols=grep("^PersonIdSHb-ab\\d$", names(DT))]

#Hb, Lch, IdS, OutS sum
DT[, "PersonHb-sum" := rowSums(.SD), .SDcols=grep("^PersonHb-\\d$", names(DT))]
DT[, "PersonOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonOutS-\\d$", names(DT))]
DT[, "PersonIdS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdS-\\d$", names(DT))]
DT[, "PersonLch-sum" := rowSums(.SD), .SDcols=grep("^PersonLch-\\d$", names(DT))]

#Proportional gap -- capped on ideal = 1, real = 0
PersonHbOutS_capped <- pmax(DT[, `PersonHbOutS-sum`], 0)
PersonIdSHb_capped <- pmax(DT[, `PersonIdSHb-sum`], 0)
DT[, "PersonProgapS-capsum" := PersonHbOutS_capped / (PersonHbOutS_capped + PersonIdSHb_capped)]
DT[`PersonOutS-sum` > `PersonIdS-sum`, "PersonProgapS-capsum" := NA] #Ideal must > real
DT[!is.finite(`PersonProgapS-capsum`), "PersonProgapS-capsum" := NA]

#Proportional gap -- no cap
DT[, "PersonProgapS-sum" := `PersonHbOutS-sum` / `PersonIdSOutS-sum`]
DT[`PersonOutS-sum` > `PersonIdS-sum`, "PersonProgapS-sum" := NA]
DT[!is.finite(`PersonProgapS-sum`), "PersonProgapS-sum" := NA]




"
### Clean temp vars and save the environment
"
rm(list=ls()[which(ls() != "DT" & ls() != "codec")]) #Preserve only DT and codec

# save.image()








"
----------------------------------------------------------------------
## Analysis
----------------------------------------------------------------------
"
"
### Top hobby categories
"
#Examine by order
DT[, .N, by=.(`Enough-2_1`)][order(-N)]

#Acquire top 6 category names
topCats <- DT[, .N, by=.(`Enough-2_1`)][order(-N)][1:6, `Enough-2_1`]




"
### ANOVA
"
#`PersonHbOutS-sum`; `PersonHbOutS-absum`; `PersonProgapS-sum`
model_anova <- aov(`PersonHbOutS-sum` ~ `Enough-2_1`, data=DT[`Enough-2_1` %in% topCats])
summary(model_anova)

ggplot(DT[`Enough-2_1` %in% topCats], aes(x=`Enough-2_1`, y=`PersonHbOutS-sum`)) +
  geom_boxplot() +
  scale_x_discrete() +
  labs(x="Hobby category", y="Personality gap", title="Personality gap by hobby category")




"
### T-test
"
#All categories
t.test(DT[, `PersonHbOutS-sum`], mu=0)
t.test(DT[, `PersonProgapS-sum`], mu=0)

#Gaming only
t.test(DT[`Enough-2_1` == "gaming", `PersonHbOutS-sum`], mu=0)
t.test(DT[`Enough-2_1` == "gaming", `PersonProgapS-sum`], mu=0)