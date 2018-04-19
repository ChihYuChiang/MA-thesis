source("analysis_preprocessing.R")

DT_1 <- getData_1()
DT_2 <- getData_2()[[2]]
DT_2_long <- getData_2()[[1]]
DT_3 <- getData_3()


"
### Study 1 (analysis 3)
"
#--hobby personality is a significant diverse from that of the controlled-context personality
t.test(DT_1[, `PersonHb-sum`], DT_1[, `PersonLch-sum`], mu=0, paired=TRUE)


#--both of the means of the hobby and news personalities are different from the general life personality
t.test(DT_1[, `PersonHb-sum`], DT_1[, `PersonOutS-sum`], mu=0, paired=TRUE)
t.test(DT_1[, `PersonLch-sum`], DT_1[, `PersonOutS-sum`], mu=0, paired=TRUE)


#--mean values of the four versions are different from each other
#ANOVA
DT_1_long <- melt(DT_1, measure.vars=c("PersonHb-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonLch-sum"), variable.name="PersonCondition", value.name="Person")

model_1_anova1 <- aov(`Person` ~ `PersonCondition` + Error(ResponseID / PersonCondition), data=DT_1_long)
summary(model_1_anova1)

#figure
ggplot(DT_1_long, aes(x=`PersonCondition`, y=`Person`)) +
  geom_boxplot() +
  labs(x="Personality version", y="Sum of personality score", title="Personality score by version")


#--The hobby-context personality improvement is significantly higher than zero
t.test(DT_1[, `PersonProgapS-sum`], mu=0)


#--personality phenomena are not predicted by the frequency of participating in the hobbies and the length of the period engaging in the hobbies
#log version, make logarithm is finite
model_1_lmlog1 <- lm(`PersonHbOutS-sum` ~ log(`Hobby-3`) + log(`Hobby-4`), data=DT_1[is.finite(log(`Hobby-3`)) & is.finite(log(`Hobby-4`))])
summary(model_1_lmlog1)

model_1_lmlog2 <- lm(`PersonProgapS-sum` ~ log(`Hobby-3`) + log(`Hobby-4`), data=DT_1[is.finite(log(`Hobby-3`)) & is.finite(log(`Hobby-4`))])
summary(model_1_lmlog2)


#--personality phenomena are not linearly correlated with the hobby’s active/passive inclination 
cor.test(DT_1[["PersonHbOutS-sum"]], DT_1[["Enough-2_1"]])
cor.test(DT_1[["PersonProgapS-sum"]], DT_1[["Enough-2_1"]])


#--top hobby count/percentage
DT_1[, .N, by=.(`Enough-2_0`)][, NP := N / nrow(DT_1)][order(-N)][1:20, ]


#--Across the top six hobbies, no evidence showed that the personality shifts were different
topC <- DT_1[, .N, by=.(`Enough-2_0`)][, NP := N / nrow(DT_1)][order(-N)][1:6, `Enough-2_0`]

#Anova
model_1_anova2 <- aov(`PersonHbOutS-sum` ~ `Enough-2_0`, data=DT_1[`Enough-2_0` %in% topC])
summary(model_1_anova2)

#figure
ggplot(DT_1[`Enough-2_0` %in% topC], aes(x=`Enough-2_0`, y=`PersonHbOutS-sum`)) +
  geom_boxplot() +
  labs(x="Hobby", y="Personality shift", title="Personality shift by top hobbies")


#--Across the top six hobbies, no evidence showed that the personality improvements were different
#Anova
model_1_anova3 <- aov(`PersonProgapS-sum` ~ `Enough-2_0`, data=DT_1[`Enough-2_0` %in% topC])
summary(model_1_anova3)

#figure
ggplot(DT_1[`Enough-2_0` %in% topC], aes(x=`Enough-2_0`, y=`PersonProgapS-sum`)) +
  geom_boxplot() +
  labs(x="Hobby", y="Personality improvement", title="Personality improvement by top hobbies")




"
### Study 2 (analysis)
"
#--The personality shift in video gaming context is significantly different from zero
t.test(DT_2[, `gap_sum`], mu=0)


#--This shift was robust across the fifty game titles
#Anova
model_2_anova <- aov(`gap_sum` ~ `core_id`, data=DT_2_long)
summary(model_2_anova)

#figure
ggplot(DT_2_long, aes(x=`core_id`, y=`gap_sum`)) +
  geom_boxplot() +
  labs(x="Game", y="Personality shift", title="Personality shift by games")


#--an individual’s preference on the specific games does not predict the shift
model_2_lm1 <- lm(gap_sum ~ preference, data=DT_2)
summary(model_2_lm1)


#--the personality shift correlates with the respondent’s satisfaction in real life 
cor.test(DT_2[["game_sum"]], DT_2[["combined_sum"]])




"
### Study 3 (analysis 2)
"
#--the mean values of the four self-report personalities were different from each other
DT_3_long <- melt(DT_3, measure.vars=c("PersonInS-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonSteS-sum"), variable.name="PersonCondition", value.name="Person")

#Anova
model_3_anova1 <- aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_long)
summary(model_3_anova1)

#figure
ggplot(DT_3_long, aes(x=`PersonCondition`, y=`Person`)) +
  geom_boxplot() +
  labs(x="Personality version", y="Sum of personality score", title="Personality score by version")


#--the personality shift in video gaming context is significantly different and higher from zero
t.test(DT_3[, `PersonInSOutS-sum`], mu=0)


#--the personality improvement in video gaming context is not significantly different and higher from zero
t.test(DT_3[, `PersonProgapS-sum`], mu=0)


#--the shift were not influenced by an individual’s fondness for video gaming in general and his/her preference on the specific games predicted the shift
model_3_lm1 <- lm(`PersonInSOutS-sum` ~ `GProfile-a1` + `PrefS-a1`, data=DT_3)
summary(model_3_lm1)


#--It was well predicted by the ideal personality but not the stereotypical one
model_3_lm2 <- lm(`PersonInS-sum` ~ `PersonOutS-sum` + `PersonIdS-sum` + `PersonSteS-sum`, data=DT_3)
summary(model_3_lm2)


#--The fellow version personalities showed a similar mean difference from the ideal and stereotype
DT_3_longFellow <- melt(DT_3, measure.vars=c("PersonInF-sum", "PersonOutF-sum", "PersonIdS-sum", "PersonSteS-sum"), variable.name="PersonCondition", value.name="Person")

#Anova
model_3_anova2 <- aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_longFellow)
summary(model_3_anova2)

#figure
ggplot(DT_3_longFellow, aes(x=`PersonCondition`, y=`Person`)) +
  geom_boxplot() +
  labs(x="Personality version", y="Sum of personality score", title="Personality score by version")


#--The fellow personality shift was higher than zero
t.test(DT_3[, `PersonInFOutF-sum`], mu=0)


#--The fellow version was well predicted by the ideal personality but not the stereotypical one
model_3_lm3<- lm(`PersonInF-sum` ~ `PersonOutF-sum` + `PersonIdS-sum` + `PersonSteS-sum`, data=DT_3)
summary(model_3_lm3)


#--The degrees of becoming a better self correlated positively with the personality shift
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["GProfile-11_2"]])


#--The degrees of becoming a different self correlated positively with the absolute personality shift
cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["GProfile-10_2"]])


#--the personality shift was negatively correlated with the respondent’s satisfaction in real life
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["SDTOut-sum"]])


#--the absolute personality shift was predicted by the general life satisfaction
model_3_lm4<- lm(`PersonInSOutS-absum` ~ `SDTOut-sum`, data=DT_3)
summary(model_3_lm4)


#--the personality improvement was predicted by the general life satisfaction
model_3_lm5<- lm(`PersonProgapS-sum` ~ `SDTOut-sum`, data=DT_3)
summary(model_3_lm5)


#--both the absolute shift and improvement predicted the satisfaction an individual acquired from the video gaming experience
model_3_lm6<- lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-absum` + `PersonProgapS-sum`, data=DT_3)
summary(model_3_lm6)

