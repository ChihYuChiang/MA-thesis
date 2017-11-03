"
----------------------------------------------------------------------
## Setup
Data of game and player are read in and matched up.

- Game release data, `release` (year), is read in as an interval variable.
- Missing values are imputed with variable mean conveniently (`star_user` and `star_GS`).
----------------------------------------------------------------------
"
"
### Packages
"
library(tidyverse)
library(data.table)
library(corrplot)
library(modelr)
library(glmnet)
library(VGAM)
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

#Core game traditional genre data
core_tGenre <- read_csv("../data/traditional_genre.csv", col_names=TRUE) %>%
  select(-X1, -group, -idTag, -game_title) %>%
  mutate(core_id = factor(core_id))
colnames(core_tGenre)[2:length(colnames(core_tGenre))] <- #Give genre columns identification
  unlist(lapply(X=colnames(core_tGenre)[2:length(colnames(core_tGenre))], function(X) {paste("tg_", X, sep="")}))

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
  left_join(core_tGenre, by=c("core_id")) %>%
  left_join(survey, by=c("core_id"), copy=FALSE)


#--Clean up unnecessary objs
rm(core_cluster, core_tsteScore, core_tGenre, survey, imputation_mean)








"
----------------------------------------------------------------------
## Variable
Acquire `player_df`; Compute and select variables to be used in models.

- Call the function to update the vars employed.
- Final response variable utilizes only `preference_3`.
- Mean-centered vars is marked with a suffix _ct.

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
`tg_x` | if belongs to traditional genre categories | binary

- Player personality:
Name | Definition | Unit
-----|------------|------
`game_xxxxx` | Big-five personality in game | Likert 1-7
`real_xxxxx` | Big-five personality in real life | Likert 1-7
`gap_xxxxx` | personality gap (game - real) | Likert 1-7
`satis_xxxxx` | SDT satisfaction in real life | Likert 1-7
`dissatis_xxxxx` | SDT dissatisfaction in real life | Likert 1-7
`combined_xxxxx` | SDT combined (previous two) satisfaction in real life | Likert 1-7

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
updateVars <- function(df.outcome="preference", df_player.outcome="game_extraversion"){
  #--Create response variable
  df <<- df %>%
    rowwise() %>% #Rowwise to make the ordinary functions work
    mutate(preference = mean(c(preference_3))) %>%
    ungroup() #Ungroup to cancel rowwise
  

  #--Mean-center predictor variables (if haven't been produced)
  if(!ncol(select(df, matches("_ct$")))) {
    df <<- mutate_at(df, vars(starts_with("tste"),
                              starts_with("game"),
                              starts_with("real"),
                              starts_with("satis"),
                              starts_with("dissatis"),
                              starts_with("combined")), funs(ct = . - mean(.)))
  }


  #--Compute personalty gap
  df <<- mutate(df,
                gap_extraversion = game_extraversion - real_extraversion,
                gap_agreeableness = game_agreeableness - real_agreeableness,
                gap_conscientiousness = game_conscientiousness - real_conscientiousness,
                gap_emotionstability = game_emotionstability - real_emotionstability,
                gap_openness = game_openness - real_openness,
                gap_sum = gap_extraversion + gap_agreeableness + gap_conscientiousness + gap_emotionstability + gap_openness,
                gap_sum_abs = abs(gap_extraversion) + abs(gap_agreeableness) + abs(gap_conscientiousness) + abs(gap_emotionstability) + abs(gap_openness),
                game_sum = game_extraversion + game_agreeableness + game_conscientiousness + game_emotionstability + game_openness,
                real_sum = real_extraversion + real_agreeableness + real_conscientiousness + real_emotionstability + real_openness,
                dissatis_sum = dissatis_autonomy + dissatis_relatedness + dissatis_competence,
                satis_sum = satis_autonomy + satis_relatedness + satis_competence,
                combined_sum = combined_autonomy + combined_relatedness + combined_competence
                )


  #--Acquire player df, key=player
  df_player <<- distinct(df, respondent, .keep_all=TRUE)
  
  
  #--Select variables to be included in regression (model formation)
  #Sets of predictor variables from file
  df_predictors <- read.csv("../data/vars/predictors.csv", header=TRUE, na.strings="")

  #Get column name as model id
  modelId <- colnames(df_predictors)
  
  #predictor variable as strings for each model
  predictorString <- apply(df_predictors, MARGIN=2, function(x) paste(na.omit(x), collapse="+"))
  
  #Make the dfs into a data frame
  dfs <<- data.frame(predictorString, row.names=modelId, stringsAsFactors=FALSE) %>%
    mutate(df_x = map(predictorString, ~ model.matrix(as.formula(paste(df.outcome, " ~ ", .x, sep="")), data=df)[, -1])) %>% #df with only predictor variables; [, -1] used to remove redundant intercept column
    mutate(df_yx = map(df_x, ~ bind_cols(select(df, df.outcome), data.frame(.x)))) #df also with outcome variables
  dfs_player <<- data.frame(predictorString, row.names=modelId, stringsAsFactors=FALSE) %>%
    mutate(df_x = map(predictorString, ~ model.matrix(as.formula(paste(df_player.outcome, " ~ ", .x, sep="")), data=df_player)[, -1])) %>% #df with only predictor variables; [, -1] used to remove redundant intercept column
    mutate(df_yx = map(df_x, ~ bind_cols(select(df_player, df_player.outcome), data.frame(.x)))) #df also with outcome variables
  
  #Set row names for reference
  row.names(dfs) <<- modelId
  row.names(dfs_player) <<- modelId
  
  
  #--Print column names for
  print(colnames(df))
  
  
  #--Save the environment objects for other projects
  save.image()
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
....Models <- function() {}




"
### Simple linear model (partial models)
"
........SimplePartial <- function() {}

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
               c_star = star_user,
               c_starGS = star_GS)

#Models with specific construct as main effect
model_control <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_")))

model_gChar_survey_mean <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("distance_survey_mean")))
model_gChar_survey_median <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("distance_survey_median")))
model_gChar_review_mean <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("probability_review_mean")))
model_gChar_review_median <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("probability_review_median")))

featureNo <- seq(2, 20)
model_gChar_tstes <- map(featureNo, ~ lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with(paste("tste_", .x, "_", sep="")))))
model_gChar_tGenre <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("tg_")))

model_personality_game <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("game")))
model_personality_real <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("real")))
model_personality_gap <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("gap")))

model_personality_satis <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("satis")))
model_personality_dissatis <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("dissatis")))
model_personality_combined <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("combined")))

#Plug in for result
#tste_2 = model_gChar_tstes[[1]]
summary(model_gChar_tstes[[10]])
summary(model_gChar_tGenre)




"
### Multivariate linear model
"
........MultivariateLinear <- function() {}

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
#`ygap_sum` models are simple linear models with the sum of 5 gaps as dependent var
#gap ~ real + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), matches("^real.+ct$"), starts_with("c_")))
model_ygap_sum <- lm(gap_sum ~ .,
                 data=select(df_player_c, gap_sum, real_sum, starts_with("c_")))

#gap ~ satis + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), starts_with("c_"), matches("^combined.+ct$")))
model_ygap_sum <- lm(gap_sum ~ .,
                 data=select(df_player_c, gap_sum, starts_with("c_"), matches("^combined.+ct$")))

#gap ~ real + satis + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), matches("^real.+ct$"), starts_with("c_"), matches("^combined.+ct$")))
model_ygap_sum <- lm(gap_sum ~ .,
                 data=select(df_player_c, gap_sum, real_sum, starts_with("c_"), matches("^combined.+ct$")))

#gap ~ real + satis + real * satis + c
model_ygap <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ . +
                   (combined_autonomy_ct + combined_relatedness_ct + combined_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct),
                 data=select(df_player_c, starts_with("gap"), matches("^real.+ct$"), starts_with("c_"), matches("^combined.+ct$")))
model_ygap_sum <- lm(gap_sum ~ . + (combined_autonomy_ct + combined_relatedness_ct + combined_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct),
                 data=select(df_player_c, gap_sum, matches("^real.+ct$"), starts_with("c_"), matches("^combined.+ct$")))
model_ygap_sum <- lm(gap_sum ~ . + (combined_autonomy_ct + combined_relatedness_ct + combined_competence_ct) * real_sum,
                 data=select(df_player_c, gap_sum, real_sum, starts_with("c_"), matches("^combined.+ct$")))

#Results of seperate models
summary(model_ygap)

#MANOVA
Anova(model_ygap)
summary(Anova(model_ygap))




"
### Tobit model
"
........Tobit <- function() {}

#Update vars
updateVars()

#Player df with control marked
df_player_c <- mutate(df_player,
                      c_age = age,
                      c_education = education,
                      c_income = income,
                      c_race = race,
                      c_sex = sex)

#--Acquire corresponding df for each game personality
#Alphabetical order for personality response vars
dfs_ygame <- list(select(df_player_c, game_p = game_agreeableness, real_p_ct = real_agreeableness_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_conscientiousness, real_p_ct = real_conscientiousness_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_emotionstability, real_p_ct = real_emotionstability_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_extraversion, real_p_ct = real_extraversion_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_openness, real_p_ct = real_openness_ct, starts_with("c_"), matches("^dissatis.*ct$")))


#--Train models
#Tobit, with upper=7, lower=1, imethod=(1, 2, 3) for dif initial values
models_ygame_tobit <- map(dfs_ygame,
                          ~ vglm(game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + dissatis_competence_ct) * real_p_ct,
                                 data=.x, family=tobit(Upper=7, Lower=1, imethod=1)))

#Lm for comparison
models_ygame_lm <- map(dfs_ygame,
                       ~ lm(game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + dissatis_competence_ct) * real_p_ct,
                            data=.x))


#--Summary
for(model in models_ygame_lm) print(summary(model))
summary(models_ygame_lm[[1]])




"
### Tobit Model (full model)
"
........TobitFull <- function() {}

#Update vars
updateVars()

#Train models
dfs_player$model_tobit <- map(dfs_player$df_yx_selected,
                              ~ vglm(game_agreeableness ~ ., data=.x, family=tobit(Upper=7, Lower=1, imethod=1)))

#Summary
for(model in dfs_player$model_tobit) print(summary(model))
summary(dfs["gap_8_i", "model_tobit"][[1]])




"
### Simple linear models (full model)
"
........SimpleFull <- function() {}

#Update vars
updateVars()

#Train models
dfs$model_lm <- map(dfs$df_yx_selected, ~ lm(preference ~ ., data=.x))
dfs_player$model_lm <- map(dfs_player$df_yx_selected, ~ lm(gap_extraversion ~ ., data=.x))
model_lm <- lm(preference ~ ., data=df_c_selected) 

#Summary
for(model in dfs_player$model_lm) print(summary(model))
summary(dfs["gap_8_i", "model_lm"][[1]])




"
### Grouped simple linear models (grouped by game characteristic)
"
........SimpleGrouped <- function() {}

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
for(i in seq(1, length(model_lm_group))) {
  print(paste("group", i, sep=" "))
  print(summary(model_lm_group[[i]]))
}




"
### Lasso and ridge

- `glmnet` alpha=1 -> lasso; alpha=0 -> ridge
"
........LassoRidge <- function() {}

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




"
### Double Lasso variable selection

- Based on paper `Using Double-Lasso Selection for Principled Variable Selection`
- by Oleg Urminsky, Christian Hansen, and Victor Chernozhukov
"
........DoubleLasso <- function() {}


#--Function for updating lambda used in selection
#n = number of observation; p = number of independent variables; se = standard error of residual or dependent variable
updateLambda <- function(n, p, se) {se * (1.1 / sqrt(n)) * qnorm(1 - (.1 / log(n)) / (2 * p))}


#--Function for acquiring the indices of the selected variables in df_x
#df_x = matrix with only variables to be tested; y = dependent variable or treatment variables; lambda = the initial lambda computed in advance 
acquireBetaIndices <- function(df_x, y, lambda, n, p) {
  #Update lambda k times, k is selected based on literature
  k <- 1
  while(k < 15) {
    model_las <- glmnet(x=df_x, y=y, alpha=1, lambda=lambda, standardize=TRUE)
    beta <- coef(model_las)
    residual.se <- sd(y - predict(model_las, df_x))
    lambda <- updateLambda(n=n, p=p, se=residual.se)
    k <- k + 1
  }
  
  #Return the variable indices with absolute value of beta > 0
  return(which(abs(beta) > 0))
}


#--Function to perform double lasso selection
#df_yx = df with all variables; outcomeVar = string of the outcome var in the df; form = a switch to decide the test and treatment vars
#output = a new df_yx with variables selected from df_yx
lassoSelect <- function(df_yx, outcomeVar, form) {
  #--Setting up
  #The df with y and treatment variables (those vars will not be tested, and will always be included in the output df)
  df_ytreatment <- switch(form,
                          "1"=select(df_yx, matches(outcomeVar), matches("^real.+\\D_ct$"), matches("^game.+\\D_ct$"), matches("^gap.+[a-z]{4}$"), matches("^tste.+\\d_ct$")),
                          "2"=select(df_yx, matches(outcomeVar)),
                          "3"=select(df_yx, matches(outcomeVar), matches(sub("game", "real", outcomeVar))))

  #The df with only the variables to be tested (those vars will be tested, and not necessarily be included in the output df)
  df_test <- switch(form,
                    "1"=data.matrix(select(df_yx, -matches(outcomeVar), -matches("^real.+\\D_ct$"), -matches("^game.+\\D_ct$"), -matches("^gap.+[a-z]{4}$"), -matches("^tste.+\\d_ct$"))),
                    "2"=data.matrix(select(df_yx, -matches(outcomeVar))),
                    "3"=data.matrix(select(df_yx, -matches(outcomeVar), -matches(sub("game", "real", outcomeVar)))))
  
  #The number of observations
  n <- nrow(df_test)
  
  #The number of variables to be tested
  p <- ncol(df_test)
  
  
  #--Select vars that predict outcome
  #Lambda is initialized as the se of residuals of a simple linear using only treatments predicting dependent variable
  #If the treatment var is NULL, use the se pf dependent var to initiate
  residual.se <- if(ncol(df_ytreatment) == 1) {sd(df_yx[[outcomeVar]])} else {sd(residuals(lm(as.formula(paste(outcomeVar, " ~ .", sep="")), data=df_ytreatment)))}
  lambda <- updateLambda(n=n, p=p, se=residual.se)
  
  #by Lasso model: dependent variable ~ test variables
  betaIndices <- acquireBetaIndices(df_x=df_test, y=df_yx[[outcomeVar]], lambda=lambda, n=n, p=p)
  
  
  #--Select vars that predict treatments
  #Each column of the treatment variables as the y in the Lasso selection
  #Starting from 2 because 1 is the dependent variable
  if(ncol(df_ytreatment) != 1) { #Run only when treatment vars not NULL
    for(i in seq(2, ncol(df_ytreatment))) {
      #Acquire target treatment variable
      treatment <- df_ytreatment[[i]]
      
      #Lambda is initialized as the se of the target treatment variable
      treatment.se <- sd(treatment)
      lambda <- updateLambda(n=n, p=p, se=treatment.se)
      
      #Acquire the indices and union the result indices of each treatment variable
      betaIndices <- union(betaIndices, acquireBetaIndices(df_x=df_test, y=treatment, lambda=lambda, n=n, p=p))
    }
  }
  
  
  #Process the result indices to remove the first term (the interaction term)
  betaIndices <- setdiff((betaIndices - 1), 0)
  
  #Bind the selected variables with dependent and treatment variables
  df_yx_selected <- cbind(df_ytreatment, df_test[, betaIndices])
  
  #Return a new df_yx with variables selected
  return(df_yx_selected)
}


#--Update vars
updateVars(df.outcome="preference", df_player.outcome="game_agreeableness")


#--Use the function to acquire the selected dfs (the new dfs can be fed into the simple linear model)
dfs$df_yx_selected <- map(dfs$df_yx, ~ lassoSelect(., outcomeVar="preference", form="1"))
dfs_player$df_yx_selected <- map(dfs_player$df_yx, ~ lassoSelect(., outcomeVar="game_agreeableness", form="3"))
df_c_selected <- lassoSelect(select(df_c, preference, starts_with("c_"), starts_with("tg_")), outcomeVar="preference", form="2")




"
### Predicting models
"
........PredictingModels <- function() {}


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
...CrossValidation <- function() {}




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
### partial linear model
"
........SimplePartial <- function() {}


#--Create cross validation datasets
df_c <- mutate(df,
               c_age = age,
               c_education = education,
               c_income = income,
               c_race = race,
               c_sex = sex,
               c_release = release,
               c_star = star_user)

df_c_tg <- select(df_c, preference, starts_with("c_"), starts_with("tg_"))
dfs_c <- map(featureNo, ~ select(df_c, preference, starts_with("c_"), starts_with(paste("tste_", .x, "_", sep=""))))

#Leave-one-out: k=nrow(df_yx)
df_yx_cv <- crossv_kfold(df_c_tg, k=100)
dfs_yx_cv <- map(dfs_c, ~ crossv_kfold(.x, k=100))


#--Train models on each training df
i <- 17
model_lm_cv <- map(df_yx_cv$train, ~ lm(preference ~ ., data=.x))
models_lm_cv <- map(dfs_yx_cv[[i]]$train, ~ lm(preference ~ ., data=.x))


#--MSE stats
#Acquire MSE of each model on training dfs
mses_lm_cv <- map2_dbl(model_lm_cv, df_yx_cv$test, mse_1)
msess_lm_cv <- map2_dbl(models_lm_cv, dfs_yx_cv[[i]]$test, mse_1)

#MSE mean
mses_lm_cv.mean <- mean(mses_lm_cv)
msess_lm_cv.mean <- mean(msess_lm_cv)

#MSE std
mses_lm_cv.std <- sd(mses_lm_cv)
msess_lm_cv.std <- sd(msess_lm_cv)

#Box plot of all MSEs
ggplot() +
  geom_boxplot(mapping=aes(x="MSE_tGenre", y=data_frame(mses_lm_cv)[[1]])) +
  geom_boxplot(mapping=aes(x="MSE_tste", y=data_frame(msess_lm_cv)[[1]])) +
  labs(title="Boxplot of MSEs",
       x=element_blank(),
       y="MSE value")




"
### Simple linear model and predicting models
"
........SimpleFullnPredicting <- function() {}


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
........LassoRidge <- function() {}


#--Create cross validation datasets
#Update vars
updateVars(update_predictors=FALSE)

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
...InformationCriteria <- function() {}




"
### BIC and BIC difference
"
........BIC <- function() {}


#--preference ~ tstes
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
dfs_real <- slice(dfs, 1:19)
dfs_realI <- slice(dfs, 20:38)
dfs_game <- slice(dfs, 39:57)
dfs_gameI <- slice(dfs, 58:76)
dfs_gap <- slice(dfs, 77:95)
dfs_gapI <- slice(dfs, 96:114)

dfs_real_dif <- slice(dfs, 2:19)
dfs_realI_dif <- slice(dfs, 21:38)
dfs_game_dif <- slice(dfs, 40:57)
dfs_gameI_dif <- slice(dfs, 59:76)
dfs_gap_dif <- slice(dfs, 78:95)
dfs_gapI_dif <- slice(dfs, 97:114)

#All models
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), BIC)) +
  labs(x="Model", y="BIC") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=row.names(dfs))

ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), BIC_dif)) +
  labs(x="Model", y="BIC difference") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=row.names(dfs)) +
  geom_hline(yintercept=0, linetype=3)

#Batch models
ggplot() +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), BIC, color="r")) +
  geom_line(data=dfs_realI, mapping=aes(seq(1, dim(dfs_realI)[1]), BIC, color="r"), alpha=0.5) +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), BIC, color="g")) +
  geom_line(data=dfs_gameI, mapping=aes(seq(1, dim(dfs_gameI)[1]), BIC, color="g"), alpha=0.5) +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), BIC, color="p")) +
  geom_line(data=dfs_gapI, mapping=aes(seq(1, dim(dfs_gapI)[1]), BIC, color="p"), alpha=0.5) +
  labs(x="Model (number of tste features)", y="BIC", title="Model BIC") +
  theme(legend.position="top", legend.direction="vertical") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap)[1]), minor_breaks=NULL, labels=seq(2, 20)) +
  scale_color_manual(name="Type of model (interaction models are light-colored)", values=c("r"="red", "g"="blue", "p"="black"),
                     labels=c("r"="tste + real + (tste*real)",
                              "g"="tste + real + game + (tste*game)",
                              "p"="tste + real + gap + (tste*gap)"))

ggplot() +
  geom_line(data=dfs_real_dif, mapping=aes(seq(1, dim(dfs_real_dif)[1]), BIC_dif, color="r")) +
  geom_line(data=dfs_realI_dif, mapping=aes(seq(1, dim(dfs_realI_dif)[1]), BIC_dif, color="r"), alpha=0.5) +
  geom_line(data=dfs_game_dif, mapping=aes(seq(1, dim(dfs_game_dif)[1]), BIC_dif, color="g")) +
  geom_line(data=dfs_gameI_dif, mapping=aes(seq(1, dim(dfs_gameI_dif)[1]), BIC_dif, color="g"), alpha=0.5) +
  geom_line(data=dfs_gap_dif, mapping=aes(seq(1, dim(dfs_gap_dif)[1]), BIC_dif, color="p")) +
  geom_line(data=dfs_gapI_dif, mapping=aes(seq(1, dim(dfs_gapI_dif)[1]), BIC_dif, color="p"), alpha=0.5) +
  labs(x="Model (number of tste features)", y="BIC difference", title="BIC difference when increasing the number of tste features") +
  theme(legend.position="top", legend.direction="vertical") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap_dif)[1]), minor_breaks=NULL, labels=seq(3, 20)) +
  scale_color_manual(name="Type of model (interaction models are light-colored)", values=c("r"="red", "g"="blue", "p"="black"),
                     labels=c("r"="tste + real + (tste*real)",
                              "g"="tste + real + game + (tste*game)",
                              "p"="tste + real + gap + (tste*gap)")) +
  geom_hline(yintercept=0, linetype=3)


#--Tobit lm comparison
BICs_ygame_tobit <- unlist(map(models_ygame_tobit, BIC))
BICs_ygame_lm <- unlist(map(models_ygame_lm, BIC))




"
### AIC and AIC difference
"
........AIC <- function() {}


#--preference ~ tstes
AICs <- unlist(map(model_gChar_tstes, AIC))
AICs_dif <- AICs[-1] - lag(AICs)[-1]

ggplot(data=as.data.frame(AICs)) +
  geom_line(mapping=aes(seq(2, 20), AICs)) +
  labs(x="Number of features", y="AIC") +
  scale_x_continuous(breaks=seq(2, 20), minor_breaks=NULL) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Model 3 = the AIC change from 2-feature to 3-feature models 
ggplot(data=as.data.frame(AICs_dif)) +
  geom_line(mapping=aes(seq(3, 20), AICs_dif)) +
  labs(x="Number of features", y="AIC difference") +
  scale_x_continuous(breaks=seq(3, 20), minor_breaks=NULL) +
  geom_hline(yintercept=0, linetype=3) +
  theme(axis.text.x=element_text(angle=45, hjust=1))


#--lm models
dfs$AIC <- unlist(map(dfs$model_lm, AIC))
dfs$AIC_dif <- dfs$AIC - lag(dfs$AIC)

#Seperate batch models from dfs
dfs_real <- slice(dfs, 1:19)
dfs_realI <- slice(dfs, 20:38)
dfs_game <- slice(dfs, 39:57)
dfs_gameI <- slice(dfs, 58:76)
dfs_gap <- slice(dfs, 77:95)
dfs_gapI <- slice(dfs, 96:114)

dfs_real_dif <- slice(dfs, 2:19)
dfs_realI_dif <- slice(dfs, 21:38)
dfs_game_dif <- slice(dfs, 40:57)
dfs_gameI_dif <- slice(dfs, 59:76)
dfs_gap_dif <- slice(dfs, 78:95)
dfs_gapI_dif <- slice(dfs, 97:114)

#All models
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), AIC)) +
  labs(x="Model", y="AIC") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=row.names(dfs)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), AIC_dif)) +
  labs(x="Model", y="AIC difference") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=row.names(dfs)) +
  geom_hline(yintercept=0, linetype=3) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Batch models
ggplot() +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), AIC, color="r")) +
  geom_line(data=dfs_realI, mapping=aes(seq(1, dim(dfs_realI)[1]), AIC, color="r"), alpha=0.5) +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), AIC, color="g")) +
  geom_line(data=dfs_gameI, mapping=aes(seq(1, dim(dfs_gameI)[1]), AIC, color="g"), alpha=0.5) +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), AIC, color="p")) +
  geom_line(data=dfs_gapI, mapping=aes(seq(1, dim(dfs_gapI)[1]), AIC, color="p"), alpha=0.5) +
  labs(x="Model (number of tste features)", y="AIC", title="Model AIC") +
  theme(legend.position="top", legend.direction="vertical") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap)[1]), minor_breaks=NULL, labels=seq(2, 20)) +
  scale_color_manual(name="Type of model (interaction models are light-colored)", values=c("r"="red", "g"="blue", "p"="black"),
                     labels=c("r"="tste + real + (tste*real)",
                              "g"="tste + real + game + (tste*game)",
                              "p"="tste + real + gap + (tste*gap)"))

ggplot() +
  geom_line(data=dfs_real_dif, mapping=aes(seq(1, dim(dfs_real_dif)[1]), AIC_dif, color="r")) +
  geom_line(data=dfs_realI_dif, mapping=aes(seq(1, dim(dfs_realI_dif)[1]), AIC_dif, color="r"), alpha=0.5) +
  geom_line(data=dfs_game_dif, mapping=aes(seq(1, dim(dfs_game_dif)[1]), AIC_dif, color="g")) +
  geom_line(data=dfs_gameI_dif, mapping=aes(seq(1, dim(dfs_gameI_dif)[1]), AIC_dif, color="g"), alpha=0.5) +
  geom_line(data=dfs_gap_dif, mapping=aes(seq(1, dim(dfs_gap_dif)[1]), AIC_dif, color="p")) +
  geom_line(data=dfs_gapI_dif, mapping=aes(seq(1, dim(dfs_gapI_dif)[1]), AIC_dif, color="p"), alpha=0.5) +
  labs(x="Model (number of tste features)", y="AIC difference", title="AIC difference when increasing the number of tste features") +
  theme(legend.position="top", legend.direction="vertical") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap_dif)[1]), minor_breaks=NULL, labels=seq(3, 20)) +
  scale_color_manual(name="Type of model (interaction models are light-colored)", values=c("r"="red", "g"="blue", "p"="black"),
                     labels=c("r"="tste + real + (tste*real)",
                              "g"="tste + real + game + (tste*game)",
                              "p"="tste + real + gap + (tste*gap)")) +
  geom_hline(yintercept=0, linetype=3)


#--Tobit lm comparison
AICs_ygame_tobit <- unlist(map(models_ygame_tobit, AIC))
AICs_ygame_lm <- unlist(map(models_ygame_lm, AIC))








"
----------------------------------------------------------------------
## Description
----------------------------------------------------------------------
"
...Description <- function() {}




"
### Descriptive stats
"
........DescriptiveStats <- function() {}

#Update vars
updateVars()

#Get stats
summary(select(df, matches("^dissatis")))


#--Distribution of real and game personality (comparison)
dist_personality <- function(personality){
  #Acquire specific column by reg
  real <- select(df_player, matches(paste("^real.*", personality, "$", sep="")))
  game <- select(df_player, matches(paste("^game.*", personality, "$", sep="")))
  
  #Plot: red bars = game personality
  ggplot(data=as.data.frame(real, game)) +
    geom_histogram(mapping=aes(x=real), binwidth=1) +
    geom_histogram(mapping=aes(x=game), binwidth=1, fill="red", alpha=0.5) +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7)) +
    labs(x="score", title=personality)
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
........Correlation <- function() {}

#Full matrix
cor(select(df, which(sapply(df, is.numeric))))

#Preference ~ game characteristics
corrplot(cor(select(df, preference, starts_with("distance_survey_mean"))),
         method="color", type="upper", addCoef.col="black", diag=FALSE, tl.srt=45, tl.cex=0.8, tl.col="black")

#Preference ~ player personality
corrplot(cor(select(df, preference, starts_with("gap"), starts_with("combined"))),
         method="color", type="upper", addCoef.col="black", diag=FALSE, tl.srt=45, tl.cex=0.8, tl.col="black")




"
### Difference between real and game personality
"
........Ttest <- function() {}

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
...RegressionAssumption <- function(){}

#Update vars
updateVars()




"
### Multicollinearity
"
........Multicollinearity <- function() {}

#VIF score (criterion: <10)
vif(dfs$model_lm[[1]])




"
### P-value adjustment
"
........Bonferroni <- function() {}

#Extract p-values for bonferroni
#Use `str(summary(model))` to see object structure
pValues_lm <- summary(dfs$model_lm[[1]])$coefficients[, 4]

#Bonferroni correction
p.adjust(pValues_lm, method=c("bonferroni"))




"
### Influential observations
"
........InfluentialObservations <- function() {}


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
........NormalDistribution <- function() {}

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
........Heteroscedasticity <- function() {}
