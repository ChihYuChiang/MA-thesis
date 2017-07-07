"
----------------------------------------------------------------------
## Setup
Data of game and player are read in and matched up.

- Game release data, `release` (year), is read in as an interval variable.
- Missing values are imputed with variable mean conveniently (`star_user` and `star_GS`).
----------------------------------------------------------------------
"
#--Package
library(tidyverse)
library(corrgram)
library(modelr)
library(glmnet)
library(randomForest)
library(e1071)
library(car)
library(pander)
set.seed(1)


#--Read in data
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
## Variable
Compute and select variables to be used in models.

- Player preference:
Name | Definition | Unit
-----|------------|------
`preference_1` | how much do you like | Likert 1-7=like
`preference_2` | how often play it | ordinary 1=never-7=everyday
`preference_3` | does it fit personal taste | Likert 1-7=fit

- Game characteristics:
Name | Definition | Unit
-----|------------|------
`distance_survey_mean_x` | group score from survey (distance from group mean in tste) | cosine distance
`distance_survey_median_x` | group score from survey (distance from group median in tste) | cosine distance
`probability_review_mean_x` | group score from review (mean probability to be categorized in the group by NN) | percentage
`probability_review_median_x` | group score from review (median probability to be categorized in the group by NN) | percentage
`group_survey` | group identity from survey | categorical 1-group number
`group_review` | group identity from review | categorical 1-group number

- Player personality:
Name | Definition | Unit
-----|------------|------
`game_xxxxx` | Big-five personality in game | Likert 1-7
`real_xxxxx` | Big-five personality in real life | Likert 1-7
`gap_xxxxx` | personality gap (game - real) | Likert 1-7
`satis_xxxxx` | SDT satisfaction in real life | Likert 1-7
`dissatis_xxxxx` | SDT dissatisfaction in real life | Likert 1-7
`combined_xxxxx` | SDT combined (previous two) dissatisfaction in real life | Likert 1-7

- Control:
Name | Definition | Unit
-----|------------|------
`age` | player age | interval
`education` | player education | ordinary 1-7=PhD
`income` | player annual household income | ordinary 1-7=over 150,000 
`sex` | player sex | categorical 1=male
`race` | player race | categorical 1-5
`release` | game release year | interval year
`star_GS` | general game quality rated by GameSpot expert | interval 0-10
`star_user` | general game quality rated by GameSpot user | interval 0-10

- Final response variable utilizes only `preference_3`.
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
## Models
Models applying the variables selected in the previous section.

- Predictor variables being used are edited through 'predictors.csv'
----------------------------------------------------------------------
"
"
### Linear models
"
#--Regression_linear_all
model_lm <- lm(preference ~ ., data=df_yx)
summary(model_lm)


#--Regression_linear_grouped
#Include `group survey` or `group review` for grouping (do not include in the csv, which makes dummies)
#Group the data into multiple dfs
df_yx_group <- df_yx %>% 
  bind_cols(data.frame(df$group_survey)) %>%
  group_by(df.group_survey) %>%
  nest()

#Train models
model_lm_group <- map2("preference ~ .", df_yx_group$data, lm)

#Print the results
for(i in seq(1, length(model_lm_group))){
  print(paste("group", i, sep=" "))
  print(summary(model_lm_group[[i]]))
}


"
### Lasso and ridge
"
#--Regression_lasso
#Acquire various lambda levels
lambdas <- 10^seq(3, -3, length=7)

#Train models on each lambda level
model_las <- glmnet(x=df_x, y=df$preference, alpha=1,
                    lambda=lambdas,
                    standardize=TRUE)

#Print the results
model_las$lambda
coef(model_las)


#--Regression_ridge
#Acquire various lambda levels
lambdas <- 10^seq(3, -3, length=7)

#Train models on each lambda level
model_rid <- glmnet(x=df_x, y=df$preference, alpha=0,
                    lambda=lambdas,
                    standardize=TRUE)

#Print the results
model_rid$lambda
coef(model_rid)
  

"
### Prediction models
"
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
## Model selection
----------------------------------------------------------------------
"
"
### Cross validation
"
#--Create leave-one-out datasets
df_yx_loo <- crossv_kfold(df_yx, k=nrow(df_yx))


#--Train models on each training df
model_lm_loo <- map(df_yx_loo$train, ~ lm(preference ~ ., data=.x))


#--MSE stats
#Acquire MSE of each model on training dfs
mses_lm_loo <- map2_dbl(model_lm_loo, df_yx_loo$test, mse)

#Box plot of all MSEs
ggplot(data = data_frame(mses_lm_loo), aes(x = "MSE (LOOSV)", y = data_frame(mses_lm_loo)[[1]])) +
  geom_boxplot() +
  labs(title = "Boxplot of MSEs",
       x = element_blank(),
       y = "MSE value")

#MSE mean
mse_lm_loo <- mean(mses_lm_loo)

#MSE std
mseSd_lm_loo <- sd(mses_lm_loo)


"
### Information criteria
"




"
----------------------------------------------------------------------
## Personality marginal effect

- At different group score levels)
----------------------------------------------------------------------
"




"
----------------------------------------------------------------------
## Description
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
## Regression assumptions
----------------------------------------------------------------------
"
#--Multicollinearity
#VIF score (criterion: <10)
vif(model_lm)


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
