library(lme4)
library(dplyr)
library(lattice)


#--Read data from other project
load("data/.RData") 
names(df)

#Examine if nested or fully crossed between factors (Tabulate the factors)
xtabs(~ respondent + title, df)


#--Model training
str_satisfaction <- select(df, matches("^combined.*ct$")) %>% names() %>% paste(collapse=" + ")
str_personality <- select(df, matches("^real.*ct$")) %>% names() %>% paste(collapse=" + ")
str_tste <- select(df, matches("^tste_5.*ct")) %>% names() %>% paste(collapse=" + ")

formula_HLM <- sprintf("preference ~ 1 + %s + %s + %s + (%s + %s) * (%s) + (1 + %s | respondent)",
                      str_satisfaction, str_personality, str_tste, str_satisfaction, str_personality, str_tste, str_tste) %>% as.formula()
model_HLM <- lmer(data=df, REML=FALSE, formula=formula_HLM)

formula_HLMa <- sprintf("preference ~ 1 + %s * (%s + %s) * (%s) + (0 + %s | respondent)",
                       str_tste, str_satisfaction, str_personality, str_tste, str_tste) %>% as.formula()
model_HLMa <- lmer(data=df, REML=FALSE, formula=formula_HLMa)

formula_HLMb <- sprintf("preference ~ 1 + %s + %s + %s + (1 | respondent)",
                        str_tste, str_satisfaction, str_personality) %>% as.formula()
model_HLMb <- lmer(data=df, REML=FALSE, formula=formula_HLMb)

#Linear model (for comparison)
formula_lm <- sprintf("preference ~ 1 + %s + %s + %s + (%s + %s) * (%s)",
                       str_satisfaction, str_personality, str_tste, str_satisfaction, str_personality, str_tste) %>% as.formula()
model_lm <- lm(data=df, formula=formula_lm)


#--Model summary
summary(model_HLM)
summary(model_lm)


#--Model performance
#Chisquare test
anova(model_HLMa, model_HLMb, model_HLM, model_lm)
anova(model_HLMb, model_HLM)


#--Parameter significance
'
Depending on the method specified, confint() computes confidence intervals by:
"profile", "Wald", "boot"
help("pvalues") formore
'
confint(model_HLM, level=0.95, method="Wald")

#If the diagnal line is streight, the profile interval can be used
#The vertical lines in the panels delimit the 50%, 80%, 90%, 95% and 99% confidence intervals
profile_HLMa <- profile(model_HLMa)
xyplot(profile_HLMa, absVal=FALSE)
confint(model_HLMc, level=0.95, method="profile")


#--Random effects
ranef(model_HLMa)
dotplot(ranef(model_HLMa, condVar=TRUE))
qqmath(ranef(model_HLMa, condVar=TRUE))
