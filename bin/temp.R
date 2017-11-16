library(tidyverse)
library(lcmm)


#--Read data from other project
load("data/.RData") 
names(df)


#--Model training
#Strings of specific constructs
str_satisfaction <- select(df, matches("^combined.*ct$")) %>% names() %>% paste(collapse=" + ")
str_personality <- select(df, matches("^real.*ct$")) %>% names() %>% paste(collapse=" + ")
str_tste <- select(df, matches("^tste_5.*ct")) %>% names() %>% paste(collapse=" + ")

#Formula to be used in the model
fml_main <- sprintf("preference ~ %s", str_satisfaction) %>% as.formula()
fml_random <- sprintf("~ 1") %>% as.formula()
fml_mixture <- sprintf("~ %s", str_satisfaction) %>% as.formula()
fml_classmb <- sprintf("~ %s", str_tste) %>% as.formula()

#Pretrain the model without latent classes (used as initial guess of the parameters)
model_0A <- hlme(fml_main,
                 random=fml_random, subject="respondent",
                 data=df, maxiter=500)

#Random grid search (based on model_0's estimate) with initial parameter value to avoid local minimum
#With less iteration
model_0B <- gridsearch(hlme(fml_main,
                            random=fml_random, subject="respondent",
                            mixture=fml_mixture, classmb=fml_classmb, ng=3,
                            data=df),
                       rep=30, maxiter=15, minit=model_0A)

#Main models
#Using the result from the grid search as the initial
model_1 <- hlme(fml_main, B=model_0A,
                random=fml_random, subject="respondent",
                mixture=fml_mixture, classmb=fml_classmb, ng=3,
                data=df, maxiter=500)
summary(model_1)

#Loglik and BIC summary of multiple models
summarytable(model_1, model_2)


#--Post training tools
#Multivariate Wald Test: significance of multiple parameters
WaldMult(model_1, pos=c(5, 6), name="test result")

#Posterior classification
postprob(model_1)

#Posterior distribution of the probability to be classified into each class
#Could the model distinguish the observation?
plot(model_1, which="postprob")

#Residual plot
#plot.hlme for help
plot(model_1, which="residuals")
