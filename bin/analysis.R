"
----------------------------------------------------------------------
Preliminary

- Game release data (year) is read in as an interval variable
----------------------------------------------------------------------
"
#--Setting up
#Package
library(tidyverse)
library(corrgram)
library(modelr)
library(glmnet)
library(randomForest)
library(e1071)
library(pander)
set.seed(1)

#Read in data
core_cluster <- read_csv("../data/core_cluster.csv", col_names=TRUE) %>%
  mutate(group_survey = factor(group_survey),
         group_review = factor(group_review),
         core_id = factor(core_id)) %>%
  select(-X1)
survey <- read_csv("../data/survey.csv", col_names=TRUE) %>%
  mutate(race = factor(race),
         sex = factor(sex),
         core_id = factor(core_id)) %>%
  select(-id)


#--Impute missing with mean
imputation_mean <- function(c){
  c[is.na(c)] <- mean(c, na.rm=TRUE)
  return(c)
}
core_cluster <- mutate_each(core_cluster,
                           funs(imputation_mean(.)),
                           star_user, star_GS)


#--Match up
df <- left_join(survey, core_cluster, by=c("core_id"), copy=FALSE)




"
----------------------------------------------------------------------
Transformation

- preference_1 = how much do you like
- preference_2 = how often play it
- preference_3 = match personal taste
----------------------------------------------------------------------
"
#--Create response variable
df <- df %>%
  rowwise() %>% 
  mutate(preference = mean(c(preference_3)))


#--Compute personalty gap
df <- mutate(df,
             gap_extraversion = game_extraversion - real_extraversion,
             gap_agreeableness = game_agreeableness - real_agreeableness,
             gap_conscientiousness = game_conscientiousness - real_conscientiousness,
             gap_emotionstability = game_emotionstability - real_emotionstability,
             gap_openness = game_openness - real_openness)


#--Select variables to be included in regression (model formation)
#predictor variables
predictors <- paste(read.csv("../data/predictors.csv", header=FALSE)[,1], collapse="+")

#df with only predictor variables
df_x <- model.matrix(as.formula(paste("preference ~ ", predictors, sep="")),
                     data=df) %>% #Define model formation and create dummies
  .[, -1] #Remove redundant interacept column

#df also with outcome variables
df_yx <- bind_cols(select(df, preference), data.frame(df_x))




"
----------------------------------------------------------------------
Models

- Predictor variables being used are edited through 'predictors.csv'
----------------------------------------------------------------------
"
#--Regression_linear_all
model_lm <- lm(preference ~ ., data=df_yx)
summary(model_lm)


#--Regression_linear_grouped


#--Regression_lasso
lambdas <- 10^seq(10, -2, length=100)
model_las <- glmnet(x=df_x, y=df$preference, alpha=1,
                    lambda=lambdas,
                    standardize=TRUE)
coef(model_las)


#--Regression_ridge
lambdas <- 10^seq(10, -2, length=100)
model_rid <- glmnet(x=df_x, y=df$preference, alpha=0,
                    lambda=lambdas,
                    standardize=TRUE)
coef(model_rid)
  

#--Random forest (bagging)
model_rf <- randomForest(preference ~ ., data=df_yx,
                         mtry=5, ntree=500)
summary(model_rf)


#--SVM (linear kernel)
costs <- 10^seq(2, -3, length=20)
model_svm <- tune.svm(preference ~ ., data=df_yx,
                  kernel="linear", range=list(cost=costs))
summary(model_svm)




"
----------------------------------------------------------------------
Model selection (cross validation)
----------------------------------------------------------------------
"
#--Create leave-one-out datasets
df_select_loo <- crossv_kfold(df_select, k = nrow(df_select))

# models_lm <- map(df_original_loo$train, ~ lm(biden ~ age + female + educ + dem + rep, data = .))
# 
# loocv_mse <- map2_dbl(loocv_models, loocv_data$test, mse)
# 
# ggplot(data = data_frame(loocv_mse), aes(x = "MSE (LOOSV)", y = data_frame(loocv_mse)[[1]])) +
#   geom_boxplot() +
#   labs(title = "Boxplot of MSEs",
#        x = element_blank(),
#        y = "MSE value")
# 
# mse_loocv <- mean(loocv_mse)
# mseSd_loocv <- sd(loocv_mse)




"
----------------------------------------------------------------------
Model selection (information criteria)
----------------------------------------------------------------------
"




"
----------------------------------------------------------------------
Description
----------------------------------------------------------------------
"
#--Descriptive stats
summary(df)


#--Correlation
#Full matrix
cor(select(df, which(sapply(df, is.numeric))))

#Preference - game characteristics
corrgram(select(df, preference, starts_with("distance_survey_mean")),
         order=NULL,
         lower.panel=panel.ellipse,
         upper.panel=panel.shade)

#Preference - player personality
corrgram(select(df, preference, starts_with("gap"), ends_with("combined")),
         order=NULL,
         lower.panel=panel.ellipse,
         upper.panel=panel.shade)




"
----------------------------------------------------------------------
Regression assumptions
----------------------------------------------------------------------
"
#--Influential observations


#--Normally distributed
# car::qqPlot(lm_1)
# 
# augment(lm_1, df) %>%
#   mutate(.student=rstudent(lm_1)) %>%
#   ggplot(aes(.student)) +
#   geom_density(adjust=.5) +
#   labs(title = "Density plot of the studentized residuals",
#        x="Studentized residuals",
#        y="Estimated density")


#--Heteroscedasticity


#--Multicollinearity