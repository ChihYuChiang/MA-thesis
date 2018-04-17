library(tidyverse)
library(data.table)

#Read in as DT
#Skip 2 for codec
DT <- fread("../data/raw_survey3/survey3_no25rule.csv", skip=2)

#Read in codec
codec <- as.data.table(t(fread("../data/raw_survey3/survey3_no25rule.csv", nrows=1)), keep.rownames=TRUE)
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
### Log transform
"
DT[, c("Hobby-3_log", "Hobby-4_log") := .(log(`Hobby-3`), log(`Hobby-4`))]




"
### Clean temp vars and save the environment
"
rm(list=ls()[which(ls() != "DT" & ls() != "codec")]) #Preserve only DT and codec

# save.image()








"
----------------------------------------------------------------------
## Description
----------------------------------------------------------------------
"
#--Age
breaks <- c(0, 1960, 1970, 1980, 1990, 2000, Inf)
(DT_age <- DT[, .(n=.N, mean=mean(`PersonHbOutS-sum`), std=var(`PersonHbOutS-sum`) %>% sqrt()), keyby=.(year=cut(`Demo-1`, breaks=breaks))])

ggplot(DT_age, aes(x=`year`, y=`mean`)) +
  geom_col() +
  scale_x_discrete(label=c("50-59", "60-69", "70-79", "80-89", "90-99")) +
  labs(x="Year of birth", y="Personality gap", title="Personality gap by year of birth")

cor(DT[["PersonHbOutS-sum"]], DT[["Demo-1"]])


#--Education
(DT_edu <- DT[, .(n=.N, mean=mean(`PersonHbOutS-sum`), std=var(`PersonHbOutS-sum`) %>% sqrt()), keyby=`Demo-2`])

ggplot(DT_edu, aes(x=`Demo-2`, y=`mean`)) +
  geom_col() +
  scale_x_continuous(breaks=DT_edu[["Demo-2"]], label=DT_edu[["Demo-2"]]) +
  labs(x="Education", y="Personality gap", title="Personality gap by education")

cor(DT[["PersonHbOutS-sum"]], DT[["Demo-2"]])


#--Ethnicity
DT_eth <- gather(DT, key="Ethnicity", value="Ethnicity_1", matches("^Demo-3_\\d$")) %>% data.table()
(DT_eth <- DT_eth[`Ethnicity_1` == 1, .(n=.N, mean=mean(`PersonHbOutS-sum`), std=var(`PersonHbOutS-sum`) %>% sqrt()), keyby=`Ethnicity`])

ggplot(DT_eth, aes(x=`Ethnicity`, y=`mean`)) +
  geom_col() +
  labs(x="Ethnicity", y="Personality gap", title="Personality gap by ethnicity")


#--Gender
(DT_gender <- DT[, .(n=.N, mean=mean(`PersonHbOutS-sum`), std=var(`PersonHbOutS-sum`) %>% sqrt()), keyby=`Demo-4`])

ggplot(DT_gender, aes(x=`Demo-4`, y=`mean`)) +
  geom_col() +
  scale_x_continuous(breaks=DT_gender[["Demo-4"]], label=DT_gender[["Demo-4"]]) +
  labs(x="Gender", y="Personality gap", title="Personality gap by gender")


#--Income
(DT_income <- DT[, .(n=.N, mean=mean(`PersonHbOutS-sum`), std=var(`PersonHbOutS-sum`) %>% sqrt()), keyby=`Demo-5`])

ggplot(DT_income, aes(x=`Demo-5`, y=`mean`)) +
  geom_col() +
  scale_x_continuous(breaks=DT_income[["Demo-5"]], label=DT_income[["Demo-5"]]) +
  labs(x="Income group", y="Personality gap", title="Personality gap by income")

cor(DT[["PersonHbOutS-sum"]], DT[["Demo-5"]])


#--Passive and active
#Active = 1, passive = 3
mean(DT[["Enough-2_1"]])

ggplot(DT, aes(x=`Enough-2_1`, y=`PersonHbOutS-sum`)) +
  geom_point() +
  geom_smooth() +
  labs(x="Degree of passivity", y="Personality gap (absolute)", title="Personality gap and passivity")

cor(DT[["PersonHbOutS-absum"]], DT[["Enough-2_1"]], use="complete.obs")








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
#--Across hobby categories
#`PersonHbOutS-sum`; `PersonHbOutS-absum`; `PersonProgapS-sum`
model_anova <- aov(`PersonHbOutS-sum` ~ `Enough-2_1`, data=DT[`Enough-2_1` %in% topCats])
summary(model_anova)

ggplot(DT[`Enough-2_1` %in% topCats], aes(x=`Enough-2_1`, y=`PersonHbOutS-sum`)) +
  geom_boxplot() +
  labs(x="Hobby category", y="Personality gap", title="Personality gap by hobby category")


#--Across personality conditions
#(Repeated measure)
DT_long <- gather(DT, key="PersonCondition", value="Person", `PersonHb-sum`, `PersonOutS-sum`, `PersonIdS-sum`, `PersonLch-sum`)

model_anova_re <- aov(`Person` ~ `PersonCondition` + Error(ResponseID / PersonCondition), data=DT_long)
summary(model_anova_re)

ggplot(DT_long, aes(x=`PersonCondition`, y=`Person`)) +
  geom_boxplot() +
  labs(x="Personality condition", y="Sum of personality score", title="Personality score by condition")




"
### T-test
"
#All categories
t.test(DT[, `PersonHbOutS-sum`], mu=0)
t.test(DT[, `PersonProgapS-sum`], mu=0)

#Gaming only
t.test(DT[`Enough-2_1` == "gaming", `PersonHbOutS-sum`], mu=0)
t.test(DT[`Enough-2_1` == "gaming", `PersonProgapS-sum`], mu=0)




"
### Linear model
"
model_lm <- lm(`PersonHbOutS-sum` ~ `PersonOutS-sum` + `Hobby-3` + `Hobby-4`, data=DT)
summary(model_lm)

#Log version
#Made sure the log result is finite
model_lm_log <- lm(`PersonHbOutS-sum` ~ `PersonOutS-sum` + `Hobby-3_log` + `Hobby-4_log`, data=DT[is.finite(`Hobby-3_log`) & is.finite(`Hobby-4_log`)])
summary(model_lm_log)
