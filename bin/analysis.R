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
library(rlist)
library(pander)
set.seed(1)


"
### Read in data
"
#--Read in
#Core game info and group distance/probability data
core_cluster <- read_csv("../data/core_cluster.csv", col_names=TRUE) %>%
  mutate(group_survey = factor(group_survey),
         group_review = factor(group_review),
         core_id = factor(core_id)) %>%
  select(-X1)

#Core game tste scores (of dif numbers of features)
core_tsteScore <- read_csv("../data/tste_concat.csv", col_names=TRUE) %>%
  select(-X1)

#Player-related survey data
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
#Main df, key=player-game pair
df <- bind_cols(core_cluster, core_tsteScore) %>%
  left_join(survey, by=c("core_id"), copy=FALSE)




"
----------------------------------------------------------------------
## Variable
Acquire `player_df`; Compute and select variables to be used in models.

- Call the function to update the vars employed.
- Final response variable utilizes only `preference_3`.

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
`tste_n_x` | group score from survey (tste), n=number of features | interval arbitrary

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
----------------------------------------------------------------------
"
updateVars <- function(){
  #--Create response variable
  df <<- df %>%
    rowwise() %>% #Rowwise to make the ordinary functions work
    mutate(preference = mean(c(preference_3))) %>%
    ungroup() #Ungroup to cancel rowwise
  
  
  #--Compute personalty gap
  df <<- mutate(df,
                gap_extraversion = game_extraversion - real_extraversion,
                gap_agreeableness = game_agreeableness - real_agreeableness,
                gap_conscientiousness = game_conscientiousness - real_conscientiousness,
                gap_emotionstability = game_emotionstability - real_emotionstability,
                gap_openness = game_openness - real_openness)
  
  
  #--Acquire player df, key=player
  df_player <<- distinct(df, respondent, .keep_all=TRUE)
  
  
  #--Select variables to be included in regression (model formation)
  #Sets of predictor variables from file
  predictors <<- read.csv("../data/vars/predictors.csv", header=TRUE, na.strings="")
  
  #Get column name as model id
  modelId <<- colnames(predictors)
  
  #predictor variable as strings for each model
  predictorString <<- apply(predictors, MARGIN=2, function(x) paste(na.omit(x), collapse="+"))
  
  #Make the dfs into a data frame
  dfs <<- data.frame(predictorString, modelId, stringsAsFactors=FALSE) %>%
    mutate(df_x = map(predictorString, ~ model.matrix(as.formula(paste("preference ~ ", .x, sep="")), data=df)[, -1])) %>% #df with only predictor variables; [, -1] used to remove redundant intercept column
    mutate(df_yx = map(df_x, ~ bind_cols(select(df, preference), data.frame(.x)))) #df also with outcome variables
  
  #Set row names for reference
  row.names(dfs) <<- modelId
}




"
----------------------------------------------------------------------
## Models
Models applying the variables selected. Two ways to select variables:

- Use `select` to include vars in the models from `df`/`df_player`.
- Edited through `predictors.csv` (for complexed interaction terms).
- Column name of `predictors.csv` = model id
----------------------------------------------------------------------
"
bm_models <- function(){}
"
### Simple linear model (partial models)
"
#Update vars
updateVars()

#Full df with control marked
df_c <- mutate(df,
               c_age = age,
               c_education = education,
               c_income = income,
               c_race = race,
               c_sex = sex,
               c_release = release,
               c_star = star_user)

#Models with specific construct as main effect
model_control <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_")))

model_gChar_survey_mean <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("distance_survey_mean")))
model_gChar_survey_median <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("distance_survey_median")))
model_gChar_review_mean <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("probability_review_mean")))
model_gChar_review_median <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("probability_review_median")))

featureNo <- seq(2, 20)
model_gChar_tstes <- map(featureNo, ~ lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with(paste("tste_", .x, "_", sep="")))))

model_personality_game <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("game")))
model_personality_real <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("real")))
model_personality_gap <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("gap")))

model_personality_satis <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("satis")))
model_personality_dissatis <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("dissatis")))
model_personality_combined <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("combined")))

#Plug in for result
#tste_2 = model_gChar_tstes[[1]]
summary(model_gChar_tstes[[2]])


"
### Multivariate linear model
"
#Update vars
updateVars()

#Player df with control marked
df_player_c <- mutate(df_player,
                      c_age = age,
                      c_education = education,
                      c_income = income,
                      c_race = race,
                      c_sex = sex)


#--Train models
#gap ~ real + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), starts_with("real"), starts_with("c_")))

#gap ~ dissatis + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), starts_with("c_"), starts_with("dissatis")))

#gap ~ real + satis + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), starts_with("real"), starts_with("c_"), starts_with("combined")))

#gap ~ real + dissatis + real * dissatis + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ . +
                   (dissatis_autonomy + dissatis_relatedness + dissatis_competence) * (real_extraversion + real_agreeableness + real_conscientiousness + real_emotionstability + real_openness),
                 data=select(df_player_c, starts_with("gap"), starts_with("real"), starts_with("c_"), starts_with("dissatis")))


#Results of seperate models
summary(model_ygap)

#MANOVA
Anova(model_ygap)
summary(Anova(model_ygap))


"
### Simple linear models (full model)
"
#Update vars
updateVars()

#Train models
dfs$model_lm <- map(dfs$df_yx, ~ lm(preference ~ ., data=.x))

#Summary
for(model in dfs$model_lm) print(summary(model))
summary(dfs["t4_r_g", "model_lm"][[1]])


"
### Grouped simple linear models (grouped by game characteristic)
"
#Update vars
updateVars()

#Include `group survey` or `group review` for grouping (do not include in the csv, which makes dummies)
#Group the data into multiple dfs
df_yx_group <- dfs$df_yx[[1]] %>% 
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
#Update vars
updateVars()

#Acquire various lambda levels (can be plugged in the `glmet` if needed; not used for now)
lambdas <- 10^seq(3, -3, length=7)

#Identify the best lambda level
#Adjust `nfolds` to increase the folds
dfs$lambda_las_best <- map(dfs$df_x, ~ cv.glmnet(x=.x, y=df$preference, alpha=1, nfolds=10)$lambda.min)

#Train model with best lambda level
#Better to standardize while the regulation counts on the units
dfs$model_las_best <- map2(dfs$df_x, dfs$lambda_las_best, ~ glmnet(x=.x, y=df$preference, alpha=1,
                                                                   lambda=.y,
                                                                   standardize=TRUE))

#Acquire the best result
dfs$model_las_coef <- map(dfs$model_las_best, coef)


#--Regression_ridge
#Update vars
updateVars()

#Identify the best lambda level
dfs$lambda_rid_best <- map(dfs$df_x, ~ cv.glmnet(x=.x, y=df$preference, alpha=0, nfolds=10)$lambda.min)

#Train model with best lambda level
dfs$model_rid_best <- map2(dfs$df_x, dfs$lambda_rid_best, ~ glmnet(x=.x, y=df$preference, alpha=0,
                                                                   lambda=.y,
                                                                   standardize=TRUE))

#Acquire the best result
dfs$model_rid_coef <- map(dfs$model_rid_best, coef)
  

"
### Predicting models
"
#--Random forest (bagging)
#Update vars
updateVars()

#Train model
dfs$model_rf <- map(dfs$df_yx, ~ randomForest(preference ~ ., data=.x,
                                              mtry=5, ntree=500))

#Print results
for(model in dfs$model_rf) print(summary(model))


#--SVM (linear kernel)
#Update vars
updateVars()

#Train model
costs <- 10^seq(2, -3, length=20)
dfs$model_svm <- map(dfs$df_yx, ~ tune.svm(preference ~ ., data=.x,
                                           kernel="linear", range=list(cost=costs)))

#Print results
for(model in dfs$model_svm) print(summary(model))




"
----------------------------------------------------------------------
## Cross validation
----------------------------------------------------------------------
"
bm_crossValidation <- function(){}
"
### MSE computation
"
#Works with simple regression, SVM, RF
mse_1 <- function(model, data_yx){
  res <- modelr:::residuals(model, data_yx)
  mean(res^2, na.rm=TRUE)
}

#Works with lasso and ridge
mse_2 <- function(model, lambda, data_y, data_x){
  pred <- predict(model, s=lambda, newx=data_x)
  mean((pred - data_y)^2, na.rm=TRUE)
}


"
### Simple linear model and predicting models
"
#--Create cross validation datasets
#Update vars
updateVars()

#Leave-one-out: k=nrow(df_yx)
df_yx_cv <- crossv_kfold(dfs$df_yx[[1]], k=10)


#--Train models on each training df
model_lm_cv <- map(df_yx_cv$train, ~ lm(preference ~ ., data=.x))


#--MSE stats
#Acquire MSE of each model on training dfs
mses_lm_cv <- map2_dbl(model_lm_cv, df_yx_cv$test, mse_1)


#Box plot of all MSEs
ggplot(data=data_frame(mses_lm_cv), aes(x="MSE (cross validation)", y=data_frame(mses_lm_cv)[[1]])) +
  geom_boxplot() +
  labs(title="Boxplot of MSEs",
       x=element_blank(),
       y="MSE value")

#MSE mean
mse_lm_cv <- mean(mses_lm_cv)

#MSE std
mseSd_lm_cv <- sd(mses_lm_cv)


"
### Lasso and ridge
"
#--Create cross validation datasets
#Update vars
updateVars()

#Leave-one-out: k=nrow(df_yx)
df_yx_cv <- crossv_kfold(dfs$df_yx[[1]], k=10)


#--Train models on each training df
model_las_cv <- map(df_yx_cv$train, ~ glmnet(x=as.matrix(select(as.data.frame(.x), -preference)),
                                               y=as.matrix(select(as.data.frame(.x), preference)),
                                               alpha=1,
                                               lambda=lambda_las_best,
                                               standardize=TRUE))


#--MSE stats
#Acquire MSE of each model on training dfs
mses_las_cv <- map2_dbl(model_las_cv, df_yx_cv$test, ~ mse_2(model=.x,
                                                                lambda=lambda_las_best,
                                                                data_x=as.matrix(select(as.data.frame(.y), -preference)),
                                                                data_y=as.matrix(select(as.data.frame(.y), preference))))

#Box plot of all MSEs
ggplot(data=data_frame(mses_las_cv), aes(x="MSE (cross validation)", y=data_frame(mses_las_cv)[[1]])) +
  geom_boxplot() +
  labs(title="Boxplot of MSEs",
       x=element_blank(),
       y="MSE value")

#MSE mean
mse_las_cv <- mean(mses_las_cv)

#MSE std
mseSd_las_cv <- sd(mses_las_cv)




"
----------------------------------------------------------------------
## Information criteria
----------------------------------------------------------------------
"
bm_infoCriteria <- function(){}
"
### BIC and BIC difference
"
#--gap ~ tstes
BICs <- unlist(map(model_gChar_tstes, BIC))
BICs_dif <- BICs[-1] - lag(BICs)[-1]

ggplot(data=as.data.frame(BICs)) +
  geom_line(mapping=aes(seq(2, 20), BICs)) +
  labs(x="Number of features", y="BIC") +
  scale_x_continuous(breaks=seq(2, 20), minor_breaks=NULL)

#Model 3 = the BIC change from 2-feature to 3-feature models 
ggplot(data=as.data.frame(BICs_dif)) +
  geom_line(mapping=aes(seq(3, 20), BICs_dif)) +
  labs(x="Number of features", y="BIC difference") +
  scale_x_continuous(breaks=seq(3, 20), minor_breaks=NULL) +
  geom_hline(yintercept=0, linetype=3)


#--lm models
dfs$BIC <- unlist(map(dfs$model_lm, BIC))
dfs$BIC_dif <- dfs$BIC - lag(dfs$BIC)

#Seperate batch models from dfs
dfs_gap <- slice(dfs, 1:19)
dfs_real <- slice(dfs, 20:38)
dfs_game <- slice(dfs, 39:57)

dfs_gap_dif <- slice(dfs, 2:19)
dfs_real_dif <- slice(dfs, 21:38)
dfs_game_dif <- slice(dfs, 40:57)

#All models
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), BIC)) +
  labs(x="Model", y="BIC") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId)

ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), BIC_dif)) +
  labs(x="Model", y="BIC difference") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId) +
  geom_hline(yintercept=0, linetype=3)

#Batch models
ggplot() +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), BIC, color="3")) +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), BIC, color="1")) +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), BIC, color="2")) +
  labs(x="Model (number of tste features)", y="BIC", title="Model BIC") +
  theme(legend.position="right", legend.direction="vertical")+
  scale_x_continuous(breaks=seq(1, dim(dfs_gap)[1]), minor_breaks=NULL, labels=seq(2, 20)) +
  scale_color_manual(name="Type of model", values=c("1"="red", "2"="blue", "3"="black"),
                     labels=c("real + real*tste", "real + game + game*tste", "real + gap + gap*tste"))

ggplot() +
  geom_line(data=dfs_gap_dif, mapping=aes(seq(1, dim(dfs_gap_dif)[1]), BIC_dif, color="3")) +
  geom_line(data=dfs_real_dif, mapping=aes(seq(1, dim(dfs_real_dif)[1]), BIC_dif, color="1")) +
  geom_line(data=dfs_game_dif, mapping=aes(seq(1, dim(dfs_game_dif)[1]), BIC_dif, color="2")) +
  labs(x="Model (number of tste features)", y="BIC difference", title="BIC difference when increasing the number of tste features") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap_dif)[1]), minor_breaks=NULL, labels=seq(3, 20)) +
  scale_color_manual(name="Type of model", values=c("1"="red", "2"="blue", "3"="black"),
                     labels=c("real + real*tste", "real + game + game*tste", "real + gap + gap*tste")) +
  geom_hline(yintercept=0, linetype=3)


"
### AIC and AIC difference
"
#--gap ~ tstes
AICs <- unlist(map(model_gChar_tstes, AIC))
AICs_dif <- AICs[-1] - lag(AICs)[-1]

ggplot(data=as.data.frame(AICs)) +
  geom_line(mapping=aes(seq(2, 20), AICs)) +
  labs(x="Number of features", y="AIC") +
  scale_x_continuous(breaks=seq(2, 20), minor_breaks=NULL)

#Model 3 = the AIC change from 2-feature to 3-feature models 
ggplot(data=as.data.frame(AICs_dif)) +
  geom_line(mapping=aes(seq(3, 20), AICs_dif)) +
  labs(x="Number of features", y="AIC difference") +
  scale_x_continuous(breaks=seq(3, 20), minor_breaks=NULL) +
  geom_hline(yintercept=0, linetype=3)


#--lm models
dfs$AIC <- unlist(map(dfs$model_lm, AIC))
dfs$AIC_dif <- dfs$AIC - lag(dfs$AIC)

#Seperate batch models from dfs
dfs_gap <- slice(dfs, 1:19)
dfs_real <- slice(dfs, 20:38)
dfs_game <- slice(dfs, 39:57)

#All models
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), AIC)) +
  labs(x="Model", y="AIC") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId)

ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), AIC_dif)) +
  labs(x="Model", y="AIC difference") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId) +
  geom_hline(yintercept=0, linetype=3)

#Batch models
ggplot() +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), AIC, color="3")) +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), AIC, color="1")) +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), AIC, color="2")) +
  labs(x="Model (number of tste features)", y="AIC", title="Model AIC") +
  theme(legend.position="right", legend.direction="vertical")+
  scale_x_continuous(breaks=seq(1, dim(dfs_gap)[1]), minor_breaks=NULL, labels=seq(2, 20)) +
  scale_color_manual(name="Type of model", values=c("1"="red", "2"="blue", "3"="black"),
                     labels=c("real + real*tste", "real + game + game*tste", "real + gap + gap*tste"))

ggplot() +
  geom_line(data=dfs_gap_dif, mapping=aes(seq(1, dim(dfs_gap_dif)[1]), AIC_dif, color="3")) +
  geom_line(data=dfs_real_dif, mapping=aes(seq(1, dim(dfs_real_dif)[1]), AIC_dif, color="1")) +
  geom_line(data=dfs_game_dif, mapping=aes(seq(1, dim(dfs_game_dif)[1]), AIC_dif, color="2")) +
  labs(x="Model (number of tste features)", y="AIC difference", title="AIC difference when increasing the number of tste features") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap_dif)[1]), minor_breaks=NULL, labels=seq(3, 20)) +
  scale_color_manual(name="Type of model", values=c("1"="red", "2"="blue", "3"="black"),
                     labels=c("real + real*tste", "real + game + game*tste", "real + gap + gap*tste")) +
  geom_hline(yintercept=0, linetype=3)




"
----------------------------------------------------------------------
## Description
----------------------------------------------------------------------
"
bm_description <- function(){}
"
### Descriptive stats
"
#Update vars
updateVars()

#Get stats
summary(df)


#--Distribution of real and game personality (comparison)
dist_personality <- function(personality){
  #Acquire specific column by reg
  real <- select(df_player, matches(paste("^real.*", personality, "$", sep="")))
  game <- select(df_player, matches(paste("^game.*", personality, "$", sep="")))
  
  #Plot: red bars = game personality
  ggplot(data=as.data.frame(real, game)) +
    geom_histogram(mapping=aes(x=real), binwidth=1) +
    geom_histogram(mapping=aes(x=game), binwidth=1, fill="red", alpha=0.5) +
    labs(x="distribution", title=personality)
}

#Call function for each personality
dist_personality("agreeableness")
dist_personality("conscientiousness")
dist_personality("emotionstability")
dist_personality("extraversion")
dist_personality("openness")


"
### Correlation
"
#Full matrix
cor(select(df, which(sapply(df, is.numeric))))

#Preference ~ game characteristics
corrgram(select(df, preference, starts_with("distance_survey_mean")),
         order=NULL,
         lower.panel=panel.ellipse,
         upper.panel=panel.shade)

#Preference ~ player personality
corrgram(select(df, preference, starts_with("gap"), ends_with("combined")),
         order=NULL,
         lower.panel=panel.ellipse,
         upper.panel=panel.shade)


"
### Difference between real and game personality
"
#Observation
mean(df_player$real_agreeableness)
mean(df_player$game_agreeableness)

#T test for each pair
#T test result dif: game - real
t.test(df_player$game_agreeableness, df_player$real_agreeableness, paired=TRUE)
t.test(df_player$game_conscientiousness, df_player$real_conscientiousness, paired=TRUE)
t.test(df_player$game_extraversion, df_player$real_extraversion, paired=TRUE)
t.test(df_player$game_emotionstability, df_player$real_emotionstability, paired=TRUE)
t.test(df_player$game_openness, df_player$real_openness, paired=TRUE)




"
----------------------------------------------------------------------
## Regression assumptions

- Applied on specific single model
----------------------------------------------------------------------
"
bm_regAssumption <- function(){}

#Update vars
updateVars()


"
### Multicollinearity
"
#VIF score (criterion: <10)
vif(dfs$model_lm[[1]])


"
### P-value adjustment
"
#Extract p-values for bonferroni
#Use `str(summary(model))` to see object structure
pValues_lm <- summary(dfs$model_lm[[1]])$coefficients[, 4]

#Bonferroni correction
p.adjust(pValues_lm, method=c("bonferroni"))


"
### Influential observations
"
#--Observe
#Add key statistics; add row name for graph reference
df_influenceDetect <- dfs$df_yx[[1]]
  rownames_to_column(var="row") %>%
  mutate(hat=hatvalues(dfs$model_lm[[1]]),
         student=rstudent(dfs$model_lm[[1]]),
         cooksd=cooks.distance(dfs$model_lm[[1]]))

#Draw bubble plot
ggplot(df_influenceDetect, aes(hat, student)) +
  geom_hline(yintercept=0, linetype=2) +
  geom_point(aes(size=cooksd), shape=1) +
  geom_text(data=df_influenceDetect %>%
              arrange(-cooksd) %>%
              slice(1:10),
            aes(label=row, color="orange")) +
  scale_size_continuous(range=c(1, 20)) +
  labs(title="Bubble plot of influential indicators",
       subtitle="Leverage, Sutentized residual, and Cook’s D (bubble size)",
       x="Leverage",
       y="Studentized residual") +
  theme(legend.position ="none")


#--Filter with separate criterion
hat <- df_influenceDetect %>%
  filter(hat > 2 * mean(hat))

student <- df_influenceDetect %>%
  filter(abs(student) > 2)

cooksd <- df_influenceDetect %>%
  filter(cooksd > 4 / (nrow(.) - (length(coef(model_lm)) - 1) - 1))

#Combine to identify problematic observations
bind_rows(hat, student, cooksd)


"
### Normally distributed
"
# car::qqPlot(lm_1)
# 
# augment(lm_1, df) %>%
#   mutate(.student=rstudent(lm_1)) %>%
#   ggplot(aes(.student)) +
#   geom_density(adjust=.5) +
#   labs(title = "Density plot of the studentized residuals",
#        x="Studentized residuals",
#        y="Estimated density")


"
### Heteroscedasticity
"
