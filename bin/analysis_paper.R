source("analysis_preprocessing.R")
library(psych)
library(lme4)
library(lavaan)
library(pbnm)
library(fastDummies)
library(splitstackshape)
library(corrplot)
library(colorspace)
library(tidyverse)

DT_1 <- getData_1()[[1]]
DT_1_active <- getData_1()[[2]]
DT_2 <- getData_2()[[2]]
DT_2_agame <- getData_2()[[3]]
DT_2_long <- getData_2()[[1]]
DT_3 <- getData_3()

PERSON_NAMES <- c("Extraversion", "Agreeableness", "Conscientiousness", "Emotion Stability", "Openness")
SATIS_NAMES <- c("Autonomy", "Relatedness", "Competence")

#--Cleaned data
#Set target variable
V_1 <- c("ResponseID",
         "PersonOutS-sum", "PersonHb-sum", "PersonIdS-sum",
         "PersonHbOutS-sum", "PersonHbOutS-absum", "PersonIdSOutS-sum", "PersonIdSOutS-absum")
V_1_rename <- c("ResponseID",
                "Person_life", "Person_hobby", "Person_ideal",
                "Person_hobbyLife", "Person_hobbyLife_abs", "Person_idealLife", "Person_idealLife_abs")
V_2 <- c("respondent",
         "real_sum", "game_sum",
         "gap_sum", "gap_sum_abs",
         "combined_sum")
V_2_rename <- c("ResponseID",
                "Person_life", "Person_hobby",
                "Person_hobbyLife", "Person_hobbyLife_abs",
                "Satis_life")
V_3 <- c("ResponseId",
         "GProfile-10_2", "GProfile-11_2",
         "PersonOutS-sum", "PersonInS-sum", "PersonIdS-sum",
         "PersonInSOutS-sum", "PersonInSOutS-absum", "PersonIdSOutS-sum", "PersonIdSOutS-absum",
         "SDTOut-sum", "SDTIn-sum", "SDTId-sum",
         "SDTIdOut-sum", "SDTIdOut-absum",
         "Enough-2_1")
V_3_rename <- c("ResponseID",
                "Self_different", "Self_better",
                "Person_life", "Person_hobby", "Person_ideal",
                "Person_hobbyLife", "Person_hobbyLife_abs", "Person_idealLife", "Person_idealLife_abs",
                "Satis_life", "Satis_hobby", "Satis_ideal",
                "Satis_idealLife", "Satis_idealLife_abs",
                "Game")

#Setup data tables
DT_1_clean <- DT_1[, match(V_1, names(DT_1)), with=FALSE]
names(DT_1_clean) <- V_1_rename
write.csv(DT_1_clean, file="../data/DT_1_clean.csv")

DT_2_clean <- DT_2[, match(V_2, names(DT_2)), with=FALSE]
names(DT_2_clean) <- V_2_rename
write.csv(DT_2_clean, file="../data/DT_2_clean.csv")

DT_3_clean <- DT_3[, match(V_3, names(DT_3)), with=FALSE]
names(DT_3_clean) <- V_3_rename
write.csv(DT_3_clean, file="../data/DT_3_clean.csv")







#----------------------------------------------------------------------

#Hypothesis

#----------------------------------------------------------------------



#--Study 1
cor.test(DT_1_clean[["Person_idealLife"]], DT_1_clean[["Person_hobbyLife"]])
cor.test(DT_1_clean[["Person_idealLife_abs"]], DT_1_clean[["Person_hobbyLife_abs"]])


#--Study 2
cor.test(DT_2_clean[["Satis_life"]], DT_2_clean[["Person_hobbyLife"]])
cor.test(DT_2_clean[["Satis_life"]], DT_2_clean[["Person_hobbyLife_abs"]])

lm(preference_1 ~ gap_sum + gap_sum_abs, DT_2_agame) %>% summary() #Pure liking, 5 game average


#--Study 3
cor.test(DT_3_clean[["Satis_life"]], DT_3_clean[["Person_idealLife"]])
cor.test(DT_3_clean[["Satis_life"]], DT_3_clean[["Person_idealLife_abs"]])

cor.test(DT_3_clean[["Person_idealLife"]], DT_3_clean[["Self_better"]])
cor.test(DT_3_clean[["Person_idealLife_abs"]], DT_3_clean[["Self_different"]])

cor.test(DT_3_clean[["Self_better"]], DT_3_clean[["Person_hobbyLife"]])
cor.test(DT_3_clean[["Self_different"]], DT_3_clean[["Person_hobbyLife_abs"]])

lm(`SDTIn-sum` ~ `PersonInSOutS-sum` + `PersonInSOutS-absum`, DT_3) %>% summary()
lm(`GProfile-a2` ~ `PersonInSOutS-sum` + `PersonInSOutS-absum`, DT_3) %>% summary()

cor.test(DT_3[["SDTIn-sum"]], DT_3[["GProfile-a2"]])


#--Path
corMatrix <- cor(DT_3[, .(`SDTOut-sum`, `PersonIdSOutS-sum`, `PersonIdSOutS-absum`,
                          `GProfile-10_2`, `GProfile-11_2`, `PersonInSOutS-sum`, `PersonInSOutS-absum`,
                          `SDTIn-sum`, `GProfile-a2`)])

#Component pool
"
`PersonIdSOutS-absum` ~ `SDTOut-sum`
`GProfile-a2` ~ `SDTIn-sum`
`GProfile-10_2` ~ `PersonIdSOutS-absum`
"

#Model
modelExp_sem <- "
`PersonIdSOutS-sum` ~ `SDTOut-sum`
`GProfile-11_2` ~ `PersonIdSOutS-sum`
`PersonInSOutS-sum` ~ `GProfile-11_2`
`PersonInSOutS-absum` ~ `GProfile-10_2`
`SDTIn-sum` ~ `PersonInSOutS-sum` + `PersonInSOutS-absum`
"

model_sem <- sem(modelExp_sem, sample.cov=corMatrix, sample.nobs=195)
summary(model_sem, standardized=F, fit=TRUE, rsquare=TRUE)


#--Experiments
cor.test(DT_3_clean[["Person_life"]], DT_3_clean[["Person_hobbyLife"]])
cor.test(DT_3_clean[["Person_life"]], DT_3_clean[["Person_hobbyLife_abs"]])

cor.test(DT_3_clean[["Person_life"]], DT_3_clean[["Satis_life"]])
cor.test(DT_3_clean[["Person_ideal"]], DT_3_clean[["Satis_ideal"]])

lm(Person_hobbyLife ~ Person_life + Satis_life, DT_3_clean) %>% summary()
lm(Person_hobbyLife_abs ~ Person_life + Satis_life, DT_3_clean) %>% summary()

lm(Person_hobbyLife ~ Person_life + (Satis_ideal - Satis_life), DT_3_clean) %>% summary()
lm(Person_hobbyLife_abs ~ Person_life + (Satis_ideal - Satis_life), DT_3_clean) %>% summary()

lm(Satis_hobby ~ Person_life + Person_hobbyLife + Person_hobbyLife_abs, DT_3_clean) %>% summary()
lm(Satis_hobby ~ Person_hobbyLife + Person_hobbyLife_abs, DT_3_clean) %>% summary()

lm(`PrefS-a2` ~ `PersonOutS-sum` + `GProfile-a2` + `SDTIn-sum`, DT_3) %>% summary()
lm(`PrefS-a2` ~ `PersonInSOutS-sum` + `PersonInSOutS-absum`, DT_3) %>% summary()

cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["PrefS-a2"]]) #Liking of the particular game
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["PrefS-a2"]]) #Liking of the particular game
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["GProfile-a2"]]) #Overall video game liking
cor.test(DT_3[["SDTIn-sum"]], DT_3[["PrefS-a2"]]) #Liking of the particular game
cor.test(DT_3[["SDTIn-sum"]], DT_3[["PrefS-5"]]) #Fittness of taste of the particular game

cor.test(DT_3[["SDTOut-sum"]], DT_3[["PersonInSOutS-sum"]])
cor.test(DT_3[["SDTOut-sum"]], DT_3[["PersonInSOutS-absum"]])

cor.test(DT_3_clean[["Self_better"]], DT_3_clean[["Satis_hobbyLife"]])
cor.test(DT_3_clean[["Self_different"]], DT_3_clean[["Person_hobbyLife_abs"]])

cor.test(DT_3_clean[["Person_ideal"]], DT_3_clean[["Satis_ideal"]])
cor.test(DT_3_clean[["Person_ideal"]], DT_3_clean[["Person_life"]])
cor.test(DT_3_clean[["Satis_ideal"]], DT_3_clean[["Satis_life"]])

m0 <- lm(Person_hobbyLife ~ Person_life + (Satis_ideal - Satis_life), DT_3_clean[Game])
m1 <- lmer(Person_hobbyLife ~ Person_life + (Satis_ideal - Satis_life) + (1|Game), data=DT_3_clean, REML=FALSE)

m0 <- lm(Person_hobbyLife_abs ~ Person_life + (Satis_ideal - Satis_life), DT_3_clean)
m1 <- lmer(Person_hobbyLife_abs ~ Person_life + (Satis_ideal - Satis_life) + (1|Game), data=DT_3_clean, REML=FALSE)

m0 <- lm(Satis_hobby ~ Person_life + Person_hobbyLife + Person_hobbyLife_abs, DT_3_clean)
m1 <- lmer(Satis_hobby ~ Person_life + Person_hobbyLife + Person_hobbyLife_abs + (Person_hobbyLife + Person_hobbyLife_abs|Game), data=DT_3_clean, REML=FALSE)

logLik(m0)
logLik(m1)

summary(m1)
pbnm(m1, m0, nsim=1000, tasks=10, cores=4, seed=1) %>% summary()

cor.test(DT_3[["GProfile-11_2"]], DT_3[["SDTIdOut-sum"]])
lm(`GProfile-10_2` ~ `PersonIdSOutS-absum`, DT_3) %>% summary()
lm(`GProfile-11_2` ~ `PersonIdSOutS-sum`, DT_3) %>% summary()

lm(`PersonInSOutS-absum` ~ `GProfile-10_2`, DT_3) %>% summary()
lm(`PersonInSOutS-sum` ~ `GProfile-11_2`, DT_3) %>% summary()

lm(`PersonIdSOutS-absum` ~ `SDTOut-sum`, DT_3) %>% summary()
lm(`PersonIdSOutS-sum` ~ `SDTOut-sum`, DT_3) %>% summary()

lm(`PrefS-a2` ~ `GProfile-10_2` + `GProfile-11_2`, DT_3) %>% summary()
lm(`GProfile-a2` ~ `GProfile-10_2` + `GProfile-11_2`, DT_3) %>% summary()
lm(`SDTIn-sum` ~ `GProfile-10_2` + `GProfile-11_2`, DT_3) %>% summary()

lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-sum` + `PersonInSOutS-absum`, DT_3) %>% summary()

lm(`SDTIn-sum` ~ `GProfile-11_2` + `PersonInSOutS-absum`, DT_3) %>% summary()
cor.test(DT_3[["GProfile-a2"]], DT_3[["PersonInSOutS-sum"]])
cor.test(DT_3[["GProfile-10_2"]], DT_3[["GProfile-11_2"]])
cor.test(DT_3[["SDTIn-sum"]], DT_3[["PersonInSOutS-absum"]])
cor.test(DT_3[["GProfile-11_2"]], DT_3[["PersonInSOutS-absum"]])
cor.test(DT_3[["GProfile-10_2"]], DT_3[["PersonInSOutS-sum"]])
ggplot(data=DT_3, aes(x=`PersonInSOutS-sum`, y=`SDTOut-sum`)) +
  geom_point() +
  geom_smooth()
ggplot(data=DT_2, aes(x=`gap_sum`, y=`combined_sum`)) +
  geom_point() +
  geom_smooth()

cor.test(DT_3_clean[["Satis_idealLife"]], DT_3_clean[["Person_idealLife"]])
cor.test(DT_3_clean[["Satis_idealLife"]], DT_3_clean[["Person_idealLife_abs"]])









#----------------------------------------------------------------------

#Study 1 (analysis 3)

#----------------------------------------------------------------------
#--Ideal personality is higher than general personality
t.test(DT_1[, `PersonIdS-sum`], DT_1[, `PersonOutS-sum`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonIdS-1`], DT_1[, `PersonOutS-1`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonIdS-2`], DT_1[, `PersonOutS-2`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonIdS-3`], DT_1[, `PersonOutS-3`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonIdS-4`], DT_1[, `PersonOutS-4`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonIdS-5`], DT_1[, `PersonOutS-5`], mu=0, paired=TRUE)


#--hobby personality is a significant diverse from that of the controlled-context personality
t.test(DT_1[, `PersonHb-sum`], DT_1[, `PersonLch-sum`], mu=0, paired=TRUE)


#--both of the means of the hobby and news personalities are different from the general life personality
t.test(DT_1[, `PersonHb-sum`], DT_1[, `PersonOutS-sum`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonLch-sum`], DT_1[, `PersonOutS-sum`], mu=0, paired=TRUE)


#--hobby personality was less positive than their ideal personality
t.test(DT_1[, `PersonIdS-sum`], DT_1[, `PersonHb-sum`], mu=0, paired=TRUE)


#--the gap between hobby personality and ideal personality was smaller than general personality - ideal or the control personality i ideal 
t.test(DT_1[, `PersonIdSHb-sum`], DT_1[, `PersonIdSOutS-sum`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonIdSHb-sum`], DT_1[, `PersonIdSLch-sum`], mu=0, paired=TRUE)


#--mean values of the four versions are different from each other
#description
describe(DT_1[, c("PersonHb-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonLch-sum")])

#ANOVA and figure (sum)
DT_1_long <- longForm4Plot(DT_1, measureVar=c("PersonHb-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonLch-sum"),
                           levels=c("PersonLch-sum", "PersonOutS-sum", "PersonHb-sum", "PersonIdS-sum"),
                           labels=c("control", "general", "hobby", "ideal"))

aov(`Person` ~ `PersonCondition` + Error(ResponseID / PersonCondition), data=DT_1_long[PersonCondition != "ideal"]) %>% summary()
personViolinPlot(DT_1_long, fileName="S1-all",
                 lab_x="Personality Version", lab_y="Sum of Personality Score", title="Personality Score by Version")

#ANOVA and figure (individual)
for(i in c(1:5)) {
  DT_1_long_indi <- longForm4Plot(DT_1, measureVar=c(sprintf("PersonHb-%s", i), sprintf("PersonOutS-%s", i), sprintf("PersonIdS-%s", i), sprintf("PersonLch-%s", i)),
                             levels=c(sprintf("PersonLch-%s", i), sprintf("PersonOutS-%s", i), sprintf("PersonHb-%s", i), sprintf("PersonIdS-%s", i)),
                             labels=c("control", "general", "hobby", "ideal"))
  
  aov(`Person` ~ `PersonCondition` + Error(ResponseID / PersonCondition), data=DT_1_long_indi[PersonCondition != "ideal"]) %>% summary() %>% print()
  personViolinPlot(DT_1_long_indi, fileName=sprintf("S1-%s", i),
                   lab_x="Personality Version", lab_y="Personality Score", title=sprintf("Personality Score by Version - %s", PERSON_NAMES[i]))
}


#--The hobby-context personality improvement is significantly higher than zero
#description
describe(DT_1[, .(`PersonHbOutS-sum`, tanh(`PersonProgapS-sum`))])

#t
t.test(DT_1[, `PersonHbOutS-sum`], mu=0)
t.test(DT_1[, `PersonProgapS-sum`], mu=0)
t.test(tanh(DT_1[, `PersonProgapS-sum`]), mu=0)


#--personality phenomena are not predicted by the frequency of participating in the hobbies and the length of the period engaging in the hobbies
#log version, make logarithm is finite
#Hobby-3 = How long ago (in years) did you first start doing your hobby
#Hobby-4 = How many times per year do you engage in your hobby
lm(`PersonHbOutS-sum` ~ log(`Hobby-3`) + log(`Hobby-4`), data=DT_1[is.finite(log(`Hobby-3`)) & is.finite(log(`Hobby-4`))]) %>% summary()
lm(`PersonHbOutS-absum` ~ log(`Hobby-3`) + log(`Hobby-4`), data=DT_1[is.finite(log(`Hobby-3`)) & is.finite(log(`Hobby-4`))]) %>% summary()

lm(`PersonProgapS-sum` ~ log(`Hobby-3`) + log(`Hobby-4`), data=DT_1[is.finite(log(`Hobby-3`)) & is.finite(log(`Hobby-4`))]) %>% summary()
lm(tanh(`PersonProgapS-sum`) ~ log(`Hobby-3`) + log(`Hobby-4`), data=DT_1[is.finite(log(`Hobby-3`)) & is.finite(log(`Hobby-4`))]) %>% summary()


#--personality phenomena are not linearly correlated with the hobby’s active/passive inclination 
cor.test(DT_1[["PersonHbOutS-sum"]], DT_1[["Enough-2_1"]])
cor.test(DT_1[["PersonProgapS-sum"]], DT_1[["Enough-2_1"]])
cor.test(tanh(DT_1[["PersonProgapS-sum"]]), DT_1[["Enough-2_1"]])


#--top hobby count/percentage
DT_1[, .N, by=.(`Enough-2_0`)][, NP := N / nrow(DT_1)][order(-N)][1:20, ]


#--Across the top six hobbies, no evidence showed that the personality shifts were different
topC <- DT_1[, .N, by=.(`Enough-2_0`)][, NP := N / nrow(DT_1)][order(-N)][1:6, `Enough-2_0`]

#Anova
aov(`PersonHbOutS-sum` ~ `Enough-2_0`, data=DT_1[`Enough-2_0` %in% topC]) %>% summary()

#figure
ggplot(DT_1[`Enough-2_0` %in% topC], aes(x=`Enough-2_0`, y=`PersonHbOutS-sum`)) +
  geom_boxplot() +
  labs(x="Hobby", y="Personality shift", title="Personality shift by top hobbies")


#--Across the top six hobbies, no evidence showed that the personality improvements were different
#Anova
aov(`PersonProgapS-sum` ~ `Enough-2_0`, data=DT_1[`Enough-2_0` %in% topC]) %>% summary()
aov(tanh(`PersonProgapS-sum`) ~ `Enough-2_0`, data=DT_1[`Enough-2_0` %in% topC]) %>% summary()

#figure
ggplot(DT_1[`Enough-2_0` %in% topC], aes(x=`Enough-2_0`, y=`PersonProgapS-sum`)) +
  geom_boxplot() +
  labs(x="Hobby", y="Relative personality shift", title="Relative personality shift by top hobbies")
ggplot(DT_1[`Enough-2_0` %in% topC], aes(x=`Enough-2_0`, y=tanh(`PersonProgapS-sum`))) +
  geom_boxplot() +
  labs(x="Hobby", y="Relative personality shift", title="Relative personality shift by top hobbies")


#--% of people had hobby personality > general personality and % had hobby personality > control personality
DT_1[`PersonHb-sum` > `PersonOutS-sum`] %>% nrow() / DT_1 %>% nrow()
DT_1[`PersonHb-sum` > `PersonLch-sum`] %>% nrow() / DT_1 %>% nrow()


#--%Rate as active
#Video game
DT_1_active[`playing video games` < 2] %>% nrow() / DT_1_active %>% nrow()

#Overall
tmp <- (DT_1_active[, c(10:95)] %>% as.matrix()) < 2
sum(tmp) / (nrow(tmp) * ncol(tmp))
mean(DT_1_active[, c(10:95)] %>% as.matrix())


#--The gap is not moderated by demographics
DT_1_dummy <- dummy_cols(DT_1, select_columns="Demo-4")
DT_1_dummy[is.na(DT_1_dummy)] <- 0
lm(`PersonHbOutS-sum` ~ `Demo-1` + `Demo-2` + `Demo-3_1` + `Demo-3_2` + `Demo-3_3` + `Demo-3_4` + `Demo-3_5` + `Demo-3_6` + `Demo-4_1` + `Demo-4_2` + `Demo-5`,
   data=DT_1_dummy) %>% summary()








#--------------------------------------------------------------------------------

#Study 2 (analysis)

#--------------------------------------------------------------------------------
#--The personality shift in video gaming context is significantly different from zero
#describe
describe(DT_2[, `gap_sum`])
describe(DT_2[, `game_sum`])
describe(DT_2[, `real_sum`])

#% of participants rated their hobby personality more positively than their general personality
DT_2[game_sum > real_sum] %>% nrow() / DT_2 %>% nrow()

#t
t.test(DT_2[["game_sum"]], DT_2[["real_sum"]], mu=0, paired=TRUE)

#figure (sum)
DT_2_longP <- longForm4Plot(DT_2, measureVar=c("game_sum", "real_sum"),
                            levels=c("real_sum", "game_sum"),
                            labels=c("general", "game"))
personViolinPlot(DT_2_longP, fileName="S2-all",
                 lab_x="Personality Version", lab_y="Sum of Personality Score", title="Personality Score by Version")

#figure (individual)
PERSON_NAMES_2 <- c("extraversion", "agreeableness", "conscientiousness", "emotionstability", "openness")
for(i in PERSON_NAMES_2) {
  DT_2_long_indi <- longForm4Plot(DT_2, measureVar=c(sprintf("game_%s", i), sprintf("real_%s", i)),
                                  levels=c(sprintf("real_%s", i), sprintf("game_%s", i)),
                                  labels=c("general", "game"))
  
  t.test(DT_2[[sprintf("game_%s", i)]], DT_2[[sprintf("real_%s", i)]], mu=0, paired=TRUE) %>% print()
  
  personViolinPlot(DT_2_long_indi, fileName=sprintf("S2-%s", which(PERSON_NAMES_2 == i)),
                   lab_x="Personality Version", lab_y="Personality Score", title=sprintf("Personality Score by Version - %s", PERSON_NAMES[which(PERSON_NAMES_2 == i)]))
}


#--This shift was robust across the fifty game titles
#Anova
aov(`gap_sum` ~ `core_id`, data=DT_2_long)
lmer(gap_sum ~ 1 + core_id + (1|respondent), data=DT_2_long) %>% summary()

#figure
ggplot(DT_2_long, aes(x=`core_id`, y=`gap_sum`)) +
  geom_boxplot() +
  labs(x="Game", y="Personality shift", title="Personality shift by games")


#--The personality difference was not significantly moderated by how much a participant liked videogames on average 
cor.test(DT_2_agame[["gap_sum"]], DT_2_agame[["preference_1"]]) #liking
cor.test(DT_2_agame[["gap_sum"]], DT_2_agame[["preference_2"]]) #how often played
cor.test(DT_2_agame[["gap_sum"]], DT_2_agame[["preference_3"]]) #fit taste


#--The abs personality difference was significantly moderated by how much a participant liked videogames on average 
cor.test(DT_2_agame[["gap_sum_abs"]], DT_2_agame[["preference_1"]]) #liking
cor.test(DT_2_agame[["gap_sum_abs"]], DT_2_agame[["preference_2"]]) #how often played
cor.test(DT_2_agame[["gap_sum_abs"]], DT_2_agame[["preference_3"]]) #fit taste


#--the personality shift does not correlates with the respondent’s satisfaction in real life
cor.test(DT_2[["gap_sum"]], DT_2[["combined_sum"]])
cor.test(DT_2[["gap_sum"]], DT_2[["combined_autonomy"]])
cor.test(DT_2[["gap_sum"]], DT_2[["combined_relatedness"]])
cor.test(DT_2[["gap_sum"]], DT_2[["combined_competence"]])

targetColName <- c("combined_sum", "combined_autonomy", "combined_relatedness", "combined_competence",
                   "gap_sum", "gap_extraversion", "gap_agreeableness", "gap_conscientiousness", "gap_emotionstability", "gap_openness")
corrplot(cor(DT_2[, targetColName, with=FALSE]),
         method="color", type="lower", addCoef.col="black", diag=FALSE, tl.srt=90, tl.cex=0.8, tl.col="black",
         cl.pos="r", col=colorRampPalette(diverge_hcl(3))(100))


#--the abs personality shift correlates with the respondent’s satisfaction in real life but plain gap doesn't
cor.test(DT_2[["gap_sum_abs"]], DT_2[["combined_sum"]])
cor.test(DT_2[["gap_sum"]], DT_2[["combined_sum"]])


#--real personality negatively correlates relative videogame personality (higher than overall = 1)
DT_2[, game_sum_higher := game_sum > mean(game_sum)]
cor.test(DT_2[["real_sum"]], as.numeric(DT_2[["game_sum_higher"]]))
cor.test(DT_2[["real_sum"]], DT_2[["gap_sum"]])


#--The gap is not moderated by demographics
DT_2_dummy <- dummy_cols(DT_2, select_columns=c("race", "sex"))
lm(`gap_sum` ~ `age` + `education` + `income` + `race_1` + `race_2` + `race_4` + `race_6` + `sex_1`,
   data=DT_2_dummy) %>% summary()


#--1: video person > general person, 0: otherwise Correlates with personality
cor.test((DT_2$game_sum > DT_2$real_sum) %>% as.numeric(), DT_2$real_sum)


#--Across genre
DT_2_longlg <- melt(DT_2_long, measure.vars = c("Action", "Adventure", "Role-Playing", "Shooter", "Simulation", "Sports", "Strategy"))
lmer(gap_sum ~ variable + (1|respondent), data=DT_2_longlg) %>% summary()








#--------------------------------------------------------------------------------

#Study 3 (analysis 2)

#--------------------------------------------------------------------------------
#--the mean values of the four self-report personalities were different from each other
#description
describe(DT_3[, c("PersonInS-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonSteS-sum")])

#Anova and figure (sum)
DT_3_long <- longForm4Plot(DT_3, measureVar=c("PersonSteS-sum", "PersonInS-sum", "PersonOutS-sum", "PersonIdS-sum"),
                           levels=c("PersonSteS-sum", "PersonOutS-sum", "PersonInS-sum", "PersonIdS-sum"),
                           labels=c("stereotype", "general", "game", "ideal"))

aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_long[PersonCondition != "stereotype"]) %>% summary()
personViolinPlot(DT_3_long, fileName="S3-all",
                 lab_x="Personality Version", lab_y="Sum of Personality Score", title="Personality Score by Version")

#ANOVA and figure (individual)
for(i in c(1:5)) {
  DT_3_long_indi <- longForm4Plot(DT_3, measureVar=c(sprintf("PersonSteS-%s", i), sprintf("PersonOutS-%s", i), sprintf("PersonInS-%s", i), sprintf("PersonIdS-%s", i)),
                                  levels=c(sprintf("PersonSteS-%s", i), sprintf("PersonOutS-%s", i), sprintf("PersonInS-%s", i), sprintf("PersonIdS-%s", i)),
                                  labels=c("stereotype", "general", "game", "ideal"))
  
  aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_long_indi[PersonCondition != "stereotype"]) %>% summary() %>% print()
  personViolinPlot(DT_3_long_indi, fileName=sprintf("S3-%s", i),
                   lab_x="Personality Version", lab_y="Personality Score", title=sprintf("Personality Score by Version - %s", PERSON_NAMES[i]))
}


#--Participants rated their hobby personality as significantly more positive than their general personality
t.test(DT_3[, `PersonInS-sum`], DT_3[, `PersonOutS-sum`], mu=0, paired=TRUE)


#--Participants’ video-gaming personality was less positive than their ideal personality
t.test(DT_3[, `PersonIdS-sum`], DT_3[, `PersonInS-sum`], mu=0, paired=TRUE)


#--the gap between video-game personality and ideal personality was smaller than for general personality
t.test(DT_3[, `PersonIdSInS-sum`], DT_3[, `PersonIdSOutS-sum`], mu=0, paired=TRUE)


#--% of participants rated their videogame personality as more positive than their general personality
DT_3[`PersonInS-sum` > `PersonOutS-sum`] %>% nrow() / DT_3 %>% nrow()


#--video-gaming personality was more positive than their perception of general public perceptions of video-gamer personality 
t.test(DT_3[, `PersonInS-sum`], DT_3[, `PersonSteS-sum`], mu=0, paired=TRUE)


#--the publics’ stereotypical video-game personality was closer to general personality than to actual video-game personality
t.test(DT_3[, `PersonOutSSteS-sum`], DT_3[, `PersonInSSteS-sum`], mu=0, paired=TRUE)


#--% of participants rated their videogame personality as more positive than the personality they believed others expect videogamers to have
DT_3[`PersonInS-sum` > `PersonSteS-sum`] %>% nrow() / DT_3 %>% nrow()


#--the personality shift in video gaming context is significantly different and higher from zero
t.test(DT_3[, `PersonInSOutS-sum`], mu=0, alternative="greater")
describe(DT_3[, `PersonInSOutS-sum`])


#--the personality improvement in video gaming context is not significantly different and higher from zero
t.test(DT_3[, `PersonProgapS-sum`], mu=0, alternative="greater")
t.test(DT_3[, tanh(`PersonProgapS-sum`)], mu=0, alternative="greater")
describe(DT_3[, tanh(`PersonProgapS-sum`)])


#--the shift were not influenced by an individual’s fondness for video gaming in general and his/her preference on the specific games predicted the shift
lm(`PersonInSOutS-sum` ~ `GProfile-5` + `GProfile-1` + `PrefS-4`, data=DT_3) %>% summary() #Frequency GProfile-1 from the first time
lm(`PersonInSOutS-sum` ~ `GProfile-a2` + `PrefS-a2`, data=DT_3) %>% summary() #Pure like

cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["GProfile-3_1"]]) #hard-core(7) or casual gamer
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["PrefS-5"]]) #Fit taste

lm(`PersonInSOutS-absum` ~ `GProfile-5` + `GProfile-1` + `PrefS-4`, data=DT_3) %>% summary() #Frequency GProfile-1 from the first time
lm(`PersonInSOutS-absum` ~ `GProfile-a2` + `PrefS-a2`, data=DT_3) %>% summary() #Pure like

cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["GProfile-3_1"]]) #hard-core(7) or casual gamer
cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["PrefS-5"]]) #Fit taste

lm(`PersonProgapS-sum` ~ `GProfile-a1` + `PrefS-a1`, data=DT_3) %>% summary()
lm(tanh(`PersonProgapS-sum`) ~ `GProfile-a1` + `PrefS-a1`, data=DT_3) %>% summary()


#--The gap is not moderated by demographics
DT_3_dummy <- cSplit(DT_3, 'Demo-3', sep=",")
DT_3_dummy[is.na(DT_3_dummy)] <- 0
DT_3_dummy <- dummy_cols(DT_3_dummy, select_columns=c("Demo-3_1", "Demo-3_2", "Demo-3_3", "Demo-4"))
DT_3_dummy[, `Demo-3_1_3` := `Demo-3_1_3` + `Demo-3_2_3`
          ][, `Demo-3_1_4` := `Demo-3_1_4` + `Demo-3_2_4`
          ][, `Demo-3_1_6` := `Demo-3_1_6` + `Demo-3_3_6`]
lm(`PersonInSOutS-sum` ~ `Demo-1` + `Demo-2` + `Demo-5` + `Demo-4_1` + `Demo-4_2` + `Demo-3_1_1` + `Demo-3_1_2` + `Demo-3_1_3` + `Demo-3_1_4` + `Demo-3_1_5` + `Demo-3_1_6`,
   data=DT_3_dummy) %>% summary()


#--the gap between the other person’s video-game personality and an ideal personality as smaller than for the other persons’ general personality
t.test(DT_3[, `PersonIdSInF-sum`], DT_3[, `PersonIdSOutF-sum`], mu=0, paired=TRUE)


#--other person’s video-gaming personality as more positive than their perception of the stereotypical video-gamer personality 
t.test(DT_3[, `PersonInFSteS-sum`], mu=0)


#--% of participants rated the other person’s videogame personality as more positive than their general personality
DT_3[`PersonInF-sum` > `PersonOutF-sum`] %>% nrow() / DT_3 %>% nrow()
DT_3[`PersonInF-sum` > `PersonSteS-sum`] %>% nrow() / DT_3 %>% nrow()


#--It was well predicted by the ideal personality but not the stereotypical one
lm(`PersonInS-sum` ~ `PersonOutS-sum` + `PersonIdS-sum` + `PersonSteS-sum`, data=DT_3) %>% summary()


#--The fellow version personalities showed a similar mean difference from the ideal and stereotype
#Description
describe(DT_3[, c("PersonInF-sum", "PersonOutF-sum", "PersonIdS-sum", "PersonSteS-sum")])

#Anova and figure (sum)
DT_3_longF <- longForm4Plot(DT_3, measureVar=c("PersonSteS-sum", "PersonInF-sum", "PersonOutF-sum", "PersonIdS-sum"),
                           levels=c("PersonSteS-sum", "PersonOutF-sum", "PersonInF-sum", "PersonIdS-sum"),
                           labels=c("stereotype", "general", "game", "ideal"))

aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_longF[PersonCondition != "stereotype"]) %>% summary()
personViolinPlot(DT_3_longF, fileName="S3-allF",
                 lab_x="Personality Version", lab_y="Sum of Personality Score", title="Fellow Personality Score by Version")

#ANOVA and figure (individual)
for(i in c(1:5)) {
  DT_3_longF_indi <- longForm4Plot(DT_3, measureVar=c(sprintf("PersonSteS-%s", i), sprintf("PersonOutF-%s", i), sprintf("PersonInF-%s", i), sprintf("PersonIdS-%s", i)),
                                  levels=c(sprintf("PersonSteS-%s", i), sprintf("PersonOutF-%s", i), sprintf("PersonInF-%s", i), sprintf("PersonIdS-%s", i)),
                                  labels=c("stereotype", "general", "game", "ideal"))
  
  aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_long_indi[PersonCondition != "stereotype"]) %>% summary() %>% print()
  personViolinPlot(DT_3_longF_indi, fileName=sprintf("S3-%sF", i),
                   lab_x="Personality Version", lab_y="Personality Score", title=sprintf("Fellow Personality Score by Version - %s", PERSON_NAMES[i]))
}


#--The fellow personality shift was higher than zero
t.test(DT_3[, `PersonInFOutF-sum`], mu=0)
t.test(DT_3[, `PersonProgapF-sum`], mu=0)
t.test(DT_3[, tanh(`PersonProgapF-sum`)], mu=0)
describe(DT_3[, `PersonInFOutF-sum`])
describe(DT_3[, tanh(`PersonProgapF-sum`)])


#--The fellow version game-general personality difference is less than self version difference
t.test(DT_3[, `PersonInSOutS-sum`], DT_3[, `PersonInFOutF-sum`], mu=0, paired=TRUE)


#--The fellow version was well predicted by the ideal personality but not the stereotypical one
lm(`PersonInF-sum` ~ `PersonOutF-sum` + `PersonIdS-sum` + `PersonSteS-sum`, data=DT_3) %>% summary()


#--Game satisfaction larger than general satisfaction
DT_3_long_satis <- longForm4Plot(DT_3, measureVar=c("SDTIn-sum", "SDTOut-sum", "SDTId-sum"),
                            levels=c("SDTOut-sum", "SDTIn-sum", "SDTId-sum"),
                            labels=c("general", "game", "ideal"))
personViolinPlot(DT_3_long_satis, fileName="S3-Sall",
                 lab_x="Satisfaction Version", lab_y="Sum of Satisfaction Score", title="Satisfaction Score by Version")

for(i in c(1:3)) {
  DT_3_long_satis_indi <- longForm4Plot(DT_3, measureVar=c(sprintf("SDTIn-%s", i), sprintf("SDTOut-%s", i), sprintf("SDTId-%s", i)),
                                   levels=c(sprintf("SDTOut-%s", i), sprintf("SDTIn-%s", i), sprintf("SDTId-%s", i)),
                                   labels=c("general", "game", "ideal"))
  
  personViolinPlot(DT_3_long_satis_indi, fileName=sprintf("S3-S%s", i),
                   lab_x="Satisfaction Version", lab_y="Satisfaction Score", title=sprintf("Satisfaction Score by Version - %s", SATIS_NAMES[i]))
}


#--The fellow-perceived gap has no moderation by how well they know the other person
#How often play together
cor.test(DT_3[["PersonInFOutF-sum"]], DT_3[["Relation-4"]])

#How familiar
cor.test(DT_3[["PersonInFOutF-sum"]], DT_3[["Relation-8"]])


#--Different motivation
#GProfile-10_2 = different person
#GProfile-11_2 = better self
t.test(DT_3[, `GProfile-11_2`], DT_3[, `GProfile-10_2`], paired=TRUE)
describe(DT_3[, .(`GProfile-11_2`, `GProfile-10_2`)])


#--The degrees of becoming a better self correlated positively with the personality shift
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["GProfile-11_2"]])
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["GProfile-10_2"]])


#--The degrees of becoming a different self correlated positively with the absolute personality shift
cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["GProfile-10_2"]])


#--the personality shift was negatively correlated with the respondent’s satisfaction in real life
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["SDTOut-sum"]])


#--the absolute personality shift was predicted by the general life satisfaction
cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["SDTOut-sum"]])


#--the personality improvement was predicted by the general life satisfaction
cor.test(tanh(DT_3[["PersonProgapS-sum"]]), DT_3[["SDTOut-sum"]])


#--both the absolute shift and improvement predicted the satisfaction an individual acquired from the video gaming experience
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-absum` + `PersonProgapS-sum`, data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-absum` + tanh(`PersonProgapS-sum`), data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-sum`, data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-absum`, data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + tanh(`PersonProgapS-sum`), data=DT_3) %>% summary()
