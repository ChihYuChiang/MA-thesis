library(tidyverse)
library(lcmm)


#--Read data from other project
load("data/.RData") 
names(df)


#--Produce formula
str_satisfaction <- select(df, matches("^combined.*ct$")) %>% names() %>% paste(collapse=" + ")
str_personality <- select(df, matches("^real.*ct$")) %>% names() %>% paste(collapse=" + ")
str_tste <- select(df, matches("^tste_8.*ct")) %>% names() %>% paste(collapse=" + ")

fml_main <- sprintf("preference ~ %s + %s", str_satisfaction, str_personality) %>% as.formula()
fml_random <- sprintf("~ 1") %>% as.formula()
fml_mixture <- sprintf("~ %s + %s", str_satisfaction, str_personality) %>% as.formula()
fml_classmb <- sprintf("~ %s", str_tste) %>% as.formula()


model_0 <- hlme(fml_main,
                random=fml_random, subject="respondent",
                data=df, maxiter=500)
model_1 <- hlme(fml_main, B=model_0,
                random=fml_random, subject="respondent",
                mixture=fml_mixture, classmb=fml_classmb, ng=3,
                data=df, maxiter=500)
summary(model_1)

summarytable(model_HLM)

WaldMult(m1a, pos = c(5, 6),
         name="interaction")