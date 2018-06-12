source("analysis_preprocessing.R")
library(psych)

PLOT_PATH <- "../report/img/"

DT_1 <- getData_1()
DT_2 <- getData_2()[[2]]
DT_2_long <- getData_2()[[1]]
DT_3 <- getData_3()


#--Cleaned data for hypothesis
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
         "SDTIdOut-sum", "SDTIdOut-absum")
V_3_rename <- c("ResponseID",
                "Self_different", "Self_better",
                "Person_life", "Person_hobby", "Person_ideal",
                "Person_hobbyLife", "Person_hobbyLife_abs", "Person_idealLife", "Person_idealLife_abs",
                "Satis_life", "Satis_hobby", "Satis_ideal",
                "Satis_idealLife", "Satis_idealLife_abs")

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




"
### Study 1 (analysis 3)
"
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

#ANOVA
DT_1_long <- melt(DT_1, measure.vars=c("PersonHb-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonLch-sum"), variable.name="PersonCondition", value.name="Person")

aov(`Person` ~ `PersonCondition` + Error(ResponseID / PersonCondition), data=DT_1_long) %>% summary()

#figure - sum
DT_1_long[, PersonConditionAvg := mean(Person), by=PersonCondition]
DT_1_long$PersonCondition <- factor(DT_1_long$PersonCondition,
                                    levels=c("PersonLch-sum", "PersonOutS-sum", "PersonHb-sum", "PersonIdS-sum"),
                                    labels=c("control", "general", "hobby", "ideal"))
ggplot(DT_1_long, aes(x=`PersonCondition`, y=`Person`)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(x="Personality version", y="Sum of personality score", title="Personality score by version")
ggsave("personality-des_all.png", device="png", path=PLOT_PATH)

#figure - individual
DT_1_long_indi <- melt(DT_1, measure.vars=c("PersonHb-5", "PersonOutS-5", "PersonIdS-5", "PersonLch-5"), variable.name="PersonCondition", value.name="Person")
DT_1_long_indi[, PersonConditionAvg := mean(Person), by=PersonCondition]
DT_1_long_indi$PersonCondition <- factor(DT_1_long_indi$PersonCondition,
                                    levels=c("PersonLch-5", "PersonOutS-5", "PersonHb-5", "PersonIdS-5"),
                                    labels=c("control", "general", "hobby", "ideal"))
ggplot(DT_1_long_indi, aes(x=`PersonCondition`, y=`Person`)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(x="Personality version", y="Personality score", title="Personality score by version - openness")
ggsave("personality-des_5.png", device="png", path=PLOT_PATH)


#--The hobby-context personality improvement is significantly higher than zero
#description
describe(DT_1[, .(`PersonHbOutS-sum`, tanh(`PersonProgapS-sum`))])

#t
t.test(DT_1[, `PersonHbOutS-sum`], mu=0, alternative="greater")
t.test(DT_1[, `PersonProgapS-sum`], mu=0, alternative="greater")
t.test(tanh(DT_1[, `PersonProgapS-sum`]), mu=0, alternative="greater")


#--personality phenomena are not predicted by the frequency of participating in the hobbies and the length of the period engaging in the hobbies
#log version, make logarithm is finite
#Hobby-3 = How long ago (in years) did you first start doing your hobby
#Hobby-4 = How many times per year do you engage in your hobby
lm(`PersonHbOutS-sum` ~ log(`Hobby-3`) + log(`Hobby-4`), data=DT_1[is.finite(log(`Hobby-3`)) & is.finite(log(`Hobby-4`))]) %>% summary()

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




"
### Study 2 (analysis)
"
#--The personality shift in video gaming context is significantly different from zero
#describe
describe(DT_2[, `gap_sum`])
describe(DT_2[, `game_sum`])
describe(DT_2[, `real_sum`])

#% of participants rated their hobby personality more positively than their general personality
DT_2[game_sum > real_sum] %>% nrow() / DT_2 %>% nrow()

#t
t.test(DT_2[, `gap_sum`], mu=0, alternative="greater")


#--an individual’s preference on the specific games does not predict the shift
lm(gap_sum ~ preference, data=DT_2) %>% summary()


#--This shift was robust across the fifty game titles
#Anova
aov(`gap_sum` ~ `core_id`, data=DT_2_long) %>% summary()

#figure
ggplot(DT_2_long, aes(x=`core_id`, y=`gap_sum`)) +
  geom_boxplot() +
  labs(x="Game", y="Personality shift", title="Personality shift by games")


#--The personality difference was not significantly moderated by how much a participant liked videogames on average 
cor.test(DT_2[["gap_sum"]], DT_2[["preference_1"]]) #liking
cor.test(DT_2[["gap_sum"]], DT_2[["preference_2"]]) #how often played
cor.test(DT_2[["gap_sum"]], DT_2[["preference_3"]]) #fit taste

cor.test(DT_2[["gap_sum"]], DT_2[, preference_1 + preference_2 + preference_3]) #fit taste


#--the personality shift does not correlates with the respondent’s satisfaction in real life
cor.test(DT_2[["gap_sum"]], DT_2[["combined_sum"]])
lm(gap_sum ~ combined_sum, data=DT_2) %>% summary()

cor.test(DT_2[["gap_sum"]], DT_2[["combined_autonomy"]])
cor.test(DT_2[["gap_sum"]], DT_2[["combined_relatedness"]])
cor.test(DT_2[["gap_sum"]], DT_2[["combined_competence"]])


#--the personality shift correlates with the respondent’s satisfaction in real life
cor.test(DT_2[["gap_sum_abs"]], DT_2[["combined_sum"]])


#--real personality negatively correlates relative videogame personality (higher than overall = 1)
DT_2[, game_sum_higher := game_sum > mean(game_sum)]
cor.test(DT_2[["real_sum"]], as.numeric(DT_2[["game_sum_higher"]]))
cor.test(DT_2[["real_sum"]], DT_2[["gap_sum"]])



"
### Study 3 (analysis 2)
"
#--the mean values of the four self-report personalities were different from each other
DT_3_long <- melt(DT_3, measure.vars=c("PersonInS-sum", "PersonOutS-sum", "PersonIdS-sum"), variable.name="PersonCondition", value.name="Person")

#description
describe(DT_3[, c("PersonInS-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonSteS-sum")])

#Anova
aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_long) %>% summary()

#figure
DT_3_long2 <- melt(DT_3, measure.vars=c("PersonInS-sum", "PersonOutS-sum", "PersonIdS-sum", "PersonSteS-sum"), variable.name="PersonCondition", value.name="Person")
DT_3_long2[, PersonConditionAvg := mean(Person), by=PersonCondition]
DT_3_long2$PersonCondition <- factor(DT_3_long2$PersonCondition,
                                         levels=c("PersonSteS-sum", "PersonOutS-sum", "PersonInS-sum", "PersonIdS-sum"),
                                         labels=c("stereotype", "general", "game", "ideal"))
ggplot(DT_3_long2, aes(x=`PersonCondition`, y=`Person`)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(x="Personality version", y="Sum of personality score", title="Personality score by version")
ggsave("3-personality-des_all.png", device="png", path=PLOT_PATH)


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
lm(`PersonInSOutS-sum` ~ `GProfile-a1` + `PrefS-a1`, data=DT_3) %>% summary()
lm(`PersonProgapS-sum` ~ `GProfile-a1` + `PrefS-a1`, data=DT_3) %>% summary()
lm(tanh(`PersonProgapS-sum`) ~ `GProfile-a1` + `PrefS-a1`, data=DT_3) %>% summary()


#--the gap between the other person’s video-game personality and an ideal personality as smaller than for the other persons’ general personality
t.test(DT_3[, `PersonIdSInF-sum`], DT_3[, `PersonIdSOutF-sum`], mu=0, paired=TRUE)


#--other person’s video-gaming personality as more positive than their perception of the stereotypical video-gamer personality 
t.test(DT_3[, `PersonInFSteS-sum`], mu=0, alternative="greater")


#--% of participants rated the other person’s videogame personality as more positive than their general personality
DT_3[`PersonInF-sum` > `PersonOutF-sum`] %>% nrow() / DT_3 %>% nrow()
DT_3[`PersonInF-sum` > `PersonSteS-sum`] %>% nrow() / DT_3 %>% nrow()


#--It was well predicted by the ideal personality but not the stereotypical one
lm(`PersonInS-sum` ~ `PersonOutS-sum` + `PersonIdS-sum` + `PersonSteS-sum`, data=DT_3) %>% summary()


#--The fellow version personalities showed a similar mean difference from the ideal and stereotype
DT_3_longFellow <- melt(DT_3, measure.vars=c("PersonInF-sum", "PersonOutF-sum", "PersonIdS-sum", "PersonSteS-sum"), variable.name="PersonCondition", value.name="Person")

#description
describe(DT_3[, c("PersonInF-sum", "PersonOutF-sum", "PersonIdS-sum", "PersonSteS-sum")])

#Anova
aov(`Person` ~ `PersonCondition` + Error(ResponseId / PersonCondition), data=DT_3_longFellow) %>% summary()

#figure
DT_3_longFellow[, PersonConditionAvg := mean(Person), by=PersonCondition]
DT_3_longFellow$PersonCondition <- factor(DT_3_longFellow$PersonCondition,
                                     levels=c("PersonSteS-sum", "PersonOutF-sum", "PersonInF-sum", "PersonIdS-sum"),
                                     labels=c("stereotype", "general", "game", "ideal"))
ggplot(DT_3_longFellow, aes(x=`PersonCondition`, y=`Person`)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(x="Personality version", y="Sum of personality score", title="Fellow personality score by version")
ggsave("3-personality-des_allf.png", device="png", path=PLOT_PATH)


#--The fellow personality shift was higher than zero
t.test(DT_3[, `PersonInFOutF-sum`], mu=0, alternative="greater")
t.test(DT_3[, `PersonProgapF-sum`], mu=0, alternative="greater")
t.test(DT_3[, tanh(`PersonProgapF-sum`)], mu=0, alternative="greater")
describe(DT_3[, `PersonInFOutF-sum`])
describe(DT_3[, tanh(`PersonProgapF-sum`)])


#--The fellow version game-general personality difference is less than self version difference
t.test(DT_3[, `PersonInSOutS-sum`], DT_3[, `PersonInFOutF-sum`], mu=0, alternative="greater")


#--The fellow version was well predicted by the ideal personality but not the stereotypical one
lm(`PersonInF-sum` ~ `PersonOutF-sum` + `PersonIdS-sum` + `PersonSteS-sum`, data=DT_3) %>% summary()

#Moderation of how well they know
lm(`PersonInF-sum` ~ `PersonOutF-sum` + `PersonIdS-sum` + `PersonSteS-sum`, data=DT_3) %>% summary()

#--Different motivation
#GProfile-10_2 = different person
#GProfile-11_2 = better self
t.test(DT_3[, `GProfile-11_2`], DT_3[, `GProfile-10_2`], paired=TRUE, alternative="greater")
describe(DT_3[, .(`GProfile-11_2`, `GProfile-10_2`)])


#--The degrees of becoming a better self correlated positively with the personality shift
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["GProfile-11_2"]])


#--The degrees of becoming a different self correlated positively with the absolute personality shift
cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["GProfile-10_2"]])


#--the personality shift was negatively correlated with the respondent’s satisfaction in real life
cor.test(DT_3[["PersonInSOutS-sum"]], DT_3[["SDTOut-sum"]])
lm(`PersonInSOutS-sum` ~ `SDTOut-sum`, data=DT_3) %>% summary()


#--the absolute personality shift was predicted by the general life satisfaction
cor.test(DT_3[["PersonInSOutS-absum"]], DT_3[["SDTOut-sum"]])
lm(`PersonInSOutS-absum` ~ `SDTOut-sum`, data=DT_3) %>% summary()


#--the personality improvement was predicted by the general life satisfaction
cor.test(tanh(DT_3[["PersonProgapS-sum"]]), DT_3[["SDTOut-sum"]])
lm(`PersonProgapS-sum` ~ `SDTOut-sum`, data=DT_3) %>% summary()
lm(tanh(`PersonProgapS-sum`) ~ `SDTOut-sum`, data=DT_3) %>% summary()


#--both the absolute shift and improvement predicted the satisfaction an individual acquired from the video gaming experience
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-absum` + `PersonProgapS-sum`, data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-absum` + tanh(`PersonProgapS-sum`), data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-sum`, data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + `PersonInSOutS-absum`, data=DT_3) %>% summary()
lm(`SDTIn-sum` ~ `SDTOut-sum` + tanh(`PersonProgapS-sum`), data=DT_3) %>% summary()
